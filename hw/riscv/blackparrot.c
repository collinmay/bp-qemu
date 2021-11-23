/*
 * QEMU BlackParrot Test Board
 *
 * Copyright (c) 2017 SiFive, Inc., 2021 Collin May
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms and conditions of the GNU General Public License,
 * version 2 or later, as published by the Free Software Foundation.
 *
 * This program is distributed in the hope it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "qemu/osdep.h"
#include "qemu/units.h"
#include "qemu/error-report.h"
#include "qapi/error.h"
#include "hw/boards.h"
#include "hw/irq.h"
#include "hw/loader.h"
#include "hw/sysbus.h"
#include "hw/qdev-properties.h"
#include "hw/char/serial.h"
#include "target/riscv/cpu.h"
#include "hw/riscv/riscv_hart.h"
#include "hw/riscv/blackparrot.h"
#include "hw/riscv/boot.h"
#include "hw/riscv/numa.h"
#include "hw/intc/riscv_aclint.h"
#include "chardev/char.h"
#include "sysemu/arch_init.h"
#include "sysemu/device_tree.h"
#include "sysemu/sysemu.h"
#include "hw/display/ramfb.h"

static const MemMapEntry blackparrot_memmap[] = {
    [BLACKPARROT_DEBUG] =       {        0x0,         0x100 },
    [BLACKPARROT_MROM] =        {     0x1000,        0xf000 },
    [BLACKPARROT_CLINT] =       {  0x2000000,       0x10000 },
    [BLACKPARROT_UART0] =       { 0x10000000,         0x100 },
    [BLACKPARROT_FW_CFG] =      { 0x10100000,          0x18 },
    [BLACKPARROT_FLASH] =       { 0x20000000,    0x60000000 },
    [BLACKPARROT_DRAM] =        { 0x80000000,           0x0 },
};

static void create_fdt(BlackParrotState *s, const MemMapEntry *memmap,
                       uint64_t mem_size, const char *cmdline, bool is_32_bit)
{
    void *fdt;
    int cpu, socket;
    MachineState *mc = MACHINE(s);
    uint64_t addr, size;
    uint32_t *clint_cells;
    unsigned long clint_addr;
    uint32_t cpu_phandle, intc_phandle;
    uint32_t phandle = 2; // we reserve 1
    char *mem_name, *cpu_name, *core_name, *intc_name;
    char *name, *clint_name, *clust_name;
    static const char * const clint_compat[2] = {
        "sifive,clint0", "riscv,clint0"
    };

    if (mc->dtb) {
        fdt = mc->fdt = load_device_tree(mc->dtb, &s->fdt_size);
        if (!fdt) {
            error_report("load_device_tree() failed");
            exit(1);
        }
        goto update_bootargs;
    } else {
        fdt = mc->fdt = create_device_tree(&s->fdt_size);
        if (!fdt) {
            error_report("create_device_tree() failed");
            exit(1);
        }
    }

    qemu_fdt_setprop_string(fdt, "/", "model", "blackparrot-test,qemu");
    qemu_fdt_setprop_string(fdt, "/", "compatible", "blackparrot-test,riscv-virtio");
    qemu_fdt_setprop_cell(fdt, "/", "#size-cells", 0x2);
    qemu_fdt_setprop_cell(fdt, "/", "#address-cells", 0x2);

    qemu_fdt_add_subnode(fdt, "/soc");
    qemu_fdt_setprop(fdt, "/soc", "ranges", NULL, 0);
    qemu_fdt_setprop_string(fdt, "/soc", "compatible", "simple-bus");
    qemu_fdt_setprop_cell(fdt, "/soc", "#size-cells", 0x2);
    qemu_fdt_setprop_cell(fdt, "/soc", "#address-cells", 0x2);

    qemu_fdt_add_subnode(fdt, "/cpus");
    qemu_fdt_setprop_cell(fdt, "/cpus", "timebase-frequency",
                          RISCV_ACLINT_DEFAULT_TIMEBASE_FREQ);
    qemu_fdt_setprop_cell(fdt, "/cpus", "#size-cells", 0x0);
    qemu_fdt_setprop_cell(fdt, "/cpus", "#address-cells", 0x1);
    qemu_fdt_add_subnode(fdt, "/cpus/cpu-map");

    for (socket = (riscv_socket_count(mc) - 1); socket >= 0; socket--) {
        clust_name = g_strdup_printf("/cpus/cpu-map/cluster%d", socket);
        qemu_fdt_add_subnode(fdt, clust_name);

        clint_cells = g_new0(uint32_t, s->soc[socket].num_harts * 4);

        for (cpu = s->soc[socket].num_harts - 1; cpu >= 0; cpu--) {
            cpu_phandle = phandle++;

            cpu_name = g_strdup_printf("/cpus/cpu@%d",
                s->soc[socket].hartid_base + cpu);
            qemu_fdt_add_subnode(fdt, cpu_name);
            if (is_32_bit) {
                qemu_fdt_setprop_string(fdt, cpu_name, "mmu-type", "riscv,sv32");
            } else {
                qemu_fdt_setprop_string(fdt, cpu_name, "mmu-type", "riscv,sv48");
            }
            name = riscv_isa_string(&s->soc[socket].harts[cpu]);
            qemu_fdt_setprop_string(fdt, cpu_name, "riscv,isa", name);
            g_free(name);
            qemu_fdt_setprop_string(fdt, cpu_name, "compatible", "riscv");
            qemu_fdt_setprop_string(fdt, cpu_name, "status", "okay");
            qemu_fdt_setprop_cell(fdt, cpu_name, "reg",
                s->soc[socket].hartid_base + cpu);
            qemu_fdt_setprop_string(fdt, cpu_name, "device_type", "cpu");
            riscv_socket_fdt_write_id(mc, fdt, cpu_name, socket);
            qemu_fdt_setprop_cell(fdt, cpu_name, "phandle", cpu_phandle);

            intc_name = g_strdup_printf("%s/interrupt-controller", cpu_name);
            qemu_fdt_add_subnode(fdt, intc_name);
            intc_phandle = phandle++;
            qemu_fdt_setprop_cell(fdt, intc_name, "phandle", intc_phandle);
            qemu_fdt_setprop_string(fdt, intc_name, "compatible",
                "riscv,cpu-intc");
            qemu_fdt_setprop(fdt, intc_name, "interrupt-controller", NULL, 0);
            qemu_fdt_setprop_cell(fdt, intc_name, "#interrupt-cells", 1);

            clint_cells[cpu * 4 + 0] = cpu_to_be32(intc_phandle);
            clint_cells[cpu * 4 + 1] = cpu_to_be32(IRQ_M_SOFT);
            clint_cells[cpu * 4 + 2] = cpu_to_be32(intc_phandle);
            clint_cells[cpu * 4 + 3] = cpu_to_be32(IRQ_M_TIMER);

            core_name = g_strdup_printf("%s/core%d", clust_name, cpu);
            qemu_fdt_add_subnode(fdt, core_name);
            qemu_fdt_setprop_cell(fdt, core_name, "cpu", cpu_phandle);

            g_free(core_name);
            g_free(intc_name);
            g_free(cpu_name);
        }

        addr = memmap[BLACKPARROT_DRAM].base + riscv_socket_mem_offset(mc, socket);
        size = riscv_socket_mem_size(mc, socket);
        mem_name = g_strdup_printf("/memory@%lx", (long)addr);
        qemu_fdt_add_subnode(fdt, mem_name);
        qemu_fdt_setprop_cells(fdt, mem_name, "reg",
            addr >> 32, addr, size >> 32, size);
        qemu_fdt_setprop_string(fdt, mem_name, "device_type", "memory");
        riscv_socket_fdt_write_id(mc, fdt, mem_name, socket);
        g_free(mem_name);

        clint_addr = memmap[BLACKPARROT_CLINT].base +
            (memmap[BLACKPARROT_CLINT].size * socket);
        clint_name = g_strdup_printf("/soc/clint@%lx", clint_addr);
        qemu_fdt_add_subnode(fdt, clint_name);
        qemu_fdt_setprop_string_array(fdt, clint_name, "compatible",
            (char **)&clint_compat, ARRAY_SIZE(clint_compat));
        qemu_fdt_setprop_cells(fdt, clint_name, "reg",
            0x0, clint_addr, 0x0, memmap[BLACKPARROT_CLINT].size);
        qemu_fdt_setprop(fdt, clint_name, "interrupts-extended",
            clint_cells, s->soc[socket].num_harts * sizeof(uint32_t) * 4);
        riscv_socket_fdt_write_id(mc, fdt, clint_name, socket);
        g_free(clint_name);

        g_free(clint_cells);
        g_free(clust_name);
    }

    riscv_socket_fdt_write_distance_matrix(mc, fdt);

    name = g_strdup_printf("/soc/uart@%lx", (long)memmap[BLACKPARROT_UART0].base);
    qemu_fdt_add_subnode(fdt, name);
    qemu_fdt_setprop_string(fdt, name, "compatible", "ns16550a");
    qemu_fdt_setprop_cells(fdt, name, "reg",
        0x0, memmap[BLACKPARROT_UART0].base,
        0x0, memmap[BLACKPARROT_UART0].size);
    qemu_fdt_setprop_cell(fdt, name, "clock-frequency", 3686400);

    qemu_fdt_add_subnode(fdt, "/chosen");
    qemu_fdt_setprop_string(fdt, "/chosen", "stdout-path", name);
    g_free(name);

update_bootargs:
    if (cmdline) {
        qemu_fdt_setprop_string(fdt, "/chosen", "bootargs", cmdline);
    }
}

static FWCfgState *create_fw_cfg(const MachineState *mc)
{
    hwaddr base = blackparrot_memmap[BLACKPARROT_FW_CFG].base;
    hwaddr size = blackparrot_memmap[BLACKPARROT_FW_CFG].size;
    FWCfgState *fw_cfg;
    char *nodename;

    fw_cfg = fw_cfg_init_mem_wide(base + 8, base, 8, base + 16,
                                  &address_space_memory);
    fw_cfg_add_i16(fw_cfg, FW_CFG_NB_CPUS, (uint16_t)mc->smp.cpus);

    nodename = g_strdup_printf("/fw-cfg@%" PRIx64, base);
    qemu_fdt_add_subnode(mc->fdt, nodename);
    qemu_fdt_setprop_string(mc->fdt, nodename,
                            "compatible", "qemu,fw-cfg-mmio");
    qemu_fdt_setprop_sized_cells(mc->fdt, nodename, "reg",
                                 2, base, 2, size);
    qemu_fdt_setprop(mc->fdt, nodename, "dma-coherent", NULL, 0);
    g_free(nodename);
    return fw_cfg;
}

static void serial_irq_request(void *opaque, int irq, int level)
{
    // nothing useful we can do here
}

static void blackparrot_machine_init(MachineState *machine)
{
    const MemMapEntry *memmap = blackparrot_memmap;
    BlackParrotState *s = BLACKPARROT_MACHINE(machine);
    MemoryRegion *system_memory = get_system_memory();
    MemoryRegion *main_mem = g_new(MemoryRegion, 1);
    MemoryRegion *flash_mem = g_new(MemoryRegion, 1);
    MemoryRegion *mask_rom = g_new(MemoryRegion, 1);
    char *soc_name;
    target_ulong start_addr = memmap[BLACKPARROT_DRAM].base;
    target_ulong firmware_end_addr, kernel_start_addr;
    uint32_t fdt_load_addr;
    uint64_t kernel_entry;
    int i, base_hartid, hart_count;

    /* Check socket count limit */
    if (BLACKPARROT_SOCKETS_MAX < riscv_socket_count(machine)) {
        error_report("number of sockets/nodes should be less than %d",
            BLACKPARROT_SOCKETS_MAX);
        exit(1);
    }

    /* Initialize sockets */
    for (i = 0; i < riscv_socket_count(machine); i++) {
        if (!riscv_socket_check_hartids(machine, i)) {
            error_report("discontinuous hartids in socket%d", i);
            exit(1);
        }

        base_hartid = riscv_socket_first_hartid(machine, i);
        if (base_hartid < 0) {
            error_report("can't find hartid base for socket%d", i);
            exit(1);
        }

        hart_count = riscv_socket_hart_count(machine, i);
        if (hart_count < 0) {
            error_report("can't find hart count for socket%d", i);
            exit(1);
        }

        soc_name = g_strdup_printf("soc%d", i);
        object_initialize_child(OBJECT(machine), soc_name, &s->soc[i],
                                TYPE_RISCV_HART_ARRAY);
        g_free(soc_name);
        object_property_set_str(OBJECT(&s->soc[i]), "cpu-type",
                                machine->cpu_type, &error_abort);
        object_property_set_int(OBJECT(&s->soc[i]), "hartid-base",
                                base_hartid, &error_abort);
        object_property_set_int(OBJECT(&s->soc[i]), "num-harts",
                                hart_count, &error_abort);
        sysbus_realize(SYS_BUS_DEVICE(&s->soc[i]), &error_abort);

        /* Per-socket CLINT */
        riscv_aclint_swi_create(
            memmap[BLACKPARROT_CLINT].base + i * memmap[BLACKPARROT_CLINT].size,
            base_hartid, hart_count, false);
        riscv_aclint_mtimer_create(
            memmap[BLACKPARROT_CLINT].base + i * memmap[BLACKPARROT_CLINT].size +
                RISCV_ACLINT_SWI_SIZE,
            RISCV_ACLINT_DEFAULT_MTIMER_SIZE, base_hartid, hart_count,
            RISCV_ACLINT_DEFAULT_MTIMECMP, RISCV_ACLINT_DEFAULT_MTIME,
            RISCV_ACLINT_DEFAULT_TIMEBASE_FREQ, true);
    }

    if (riscv_is_32bit(&s->soc[0])) {
#if HOST_LONG_BITS == 64
        /* limit RAM size in a 32-bit system */
        if (machine->ram_size > 10 * GiB) {
            machine->ram_size = 10 * GiB;
            error_report("Limiting RAM size to 10 GiB");
        }
#endif
    } else {
    }

    /* register system main memory (actual RAM) */
    memory_region_init_ram(main_mem, NULL, "blackparrot_board.ram",
                           machine->ram_size, &error_fatal);
    memory_region_add_subregion(system_memory, memmap[BLACKPARROT_DRAM].base,
        main_mem);

    /* create device tree */
    create_fdt(s, memmap, machine->ram_size, machine->kernel_cmdline,
               riscv_is_32bit(&s->soc[0]));

    /* boot rom */
    memory_region_init_rom(mask_rom, NULL, "blackparrot_board.mrom",
                           memmap[BLACKPARROT_MROM].size, &error_fatal);
    memory_region_add_subregion(system_memory, memmap[BLACKPARROT_MROM].base,
                                mask_rom);

    if (riscv_is_32bit(&s->soc[0])) {
        firmware_end_addr = riscv_find_and_load_firmware(machine,
                                    RISCV32_BIOS_BIN, start_addr, NULL);
    } else {
        firmware_end_addr = riscv_find_and_load_firmware(machine,
                                    RISCV64_BIOS_BIN, start_addr, NULL);
    }

    if (machine->kernel_filename) {
        kernel_start_addr = riscv_calc_kernel_start_addr(&s->soc[0],
                                                         firmware_end_addr);

        kernel_entry = riscv_load_kernel(machine->kernel_filename,
                                         kernel_start_addr, NULL);

        if (machine->initrd_filename) {
            memory_region_init_ram(flash_mem, NULL, "blackparrot_board.flash", memmap[BLACKPARROT_FLASH].size, &error_fatal);
            memory_region_add_subregion(system_memory, memmap[BLACKPARROT_FLASH].base, flash_mem);

            load_image_targphys(machine->initrd_filename, memmap[BLACKPARROT_FLASH].base, memmap[BLACKPARROT_FLASH].size);

            char *mtd_name = g_strdup_printf("/soc/flash@%lx", (long) memmap[BLACKPARROT_FLASH].base);
            qemu_fdt_add_subnode(machine->fdt, mtd_name);
            qemu_fdt_setprop_string(machine->fdt, mtd_name, "compatible", "mtd-ram");
            qemu_fdt_setprop_cells(machine->fdt, mtd_name, "reg", memmap[BLACKPARROT_FLASH].base >> 32, memmap[BLACKPARROT_FLASH].base, memmap[BLACKPARROT_FLASH].size >> 32, memmap[BLACKPARROT_FLASH].size);
            qemu_fdt_setprop_cell(machine->fdt, mtd_name, "bank-width", 1);
            qemu_fdt_setprop_cell(machine->fdt, mtd_name, "memory-region", 1);
            g_free(mtd_name);
        }
    } else {
       /*
        * If dynamic firmware is used, it doesn't know where is the next mode
        * if kernel argument is not set.
        */
        kernel_entry = 0;
    }

    /*
     * Init fw_cfg.  Must be done before riscv_load_fdt, otherwise the device
     * tree cannot be altered and we get FDT_ERR_NOSPACE.
     */
    s->fw_cfg = create_fw_cfg(machine);
    rom_set_fw(s->fw_cfg);

    /* Compute the fdt load address in dram */
    fdt_load_addr = riscv_load_fdt(memmap[BLACKPARROT_DRAM].base,
                                   machine->ram_size, machine->fdt);
    /* load the reset vector */
    riscv_setup_rom_reset_vec(machine, &s->soc[0], start_addr,
                              blackparrot_memmap[BLACKPARROT_MROM].base,
                              blackparrot_memmap[BLACKPARROT_MROM].size, kernel_entry,
                              fdt_load_addr, machine->fdt);

    qemu_irq serial_irq = qemu_allocate_irq(serial_irq_request, NULL, IRQ_S_EXT);

    serial_mm_init(system_memory, memmap[BLACKPARROT_UART0].base,
        0, serial_irq, 399193,
        serial_hd(0), DEVICE_LITTLE_ENDIAN);
}

static void blackparrot_machine_instance_init(Object *obj)
{
}

static void blackparrot_machine_class_init(ObjectClass *oc, void *data)
{
    MachineClass *mc = MACHINE_CLASS(oc);

    mc->desc = "BlackParrot test board";
    mc->init = blackparrot_machine_init;
    mc->max_cpus = BLACKPARROT_CPUS_MAX;
    mc->default_cpu_type = TYPE_RISCV_CPU_BASE;
    mc->pci_allow_0_address = true;
    mc->possible_cpu_arch_ids = riscv_numa_possible_cpu_arch_ids;
    mc->cpu_index_to_instance_props = riscv_numa_cpu_index_to_props;
    mc->get_default_cpu_node_id = riscv_numa_get_default_cpu_node_id;
    mc->numa_mem_supported = true;

    machine_class_allow_dynamic_sysbus_dev(mc, TYPE_RAMFB_DEVICE);
}

static const TypeInfo blackparrot_machine_typeinfo = {
    .name       = MACHINE_TYPE_NAME("blackparrot"),
    .parent     = TYPE_MACHINE,
    .class_init = blackparrot_machine_class_init,
    .instance_init = blackparrot_machine_instance_init,
    .instance_size = sizeof(BlackParrotState),
};

static void blackparrot_machine_init_register_types(void)
{
    type_register_static(&blackparrot_machine_typeinfo);
}

type_init(blackparrot_machine_init_register_types)
