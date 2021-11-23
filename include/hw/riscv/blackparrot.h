/*
 * QEMU BlackParrot test board machine interface
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

#ifndef HW_BLACKPARROT_H
#define HW_BLACKPARROT_H

#include "hw/riscv/riscv_hart.h"
#include "hw/sysbus.h"
#include "hw/block/flash.h"
#include "qom/object.h"

#define BLACKPARROT_CPUS_MAX 32
#define BLACKPARROT_SOCKETS_MAX 8

#define TYPE_BLACKPARROT_MACHINE MACHINE_TYPE_NAME("blackparrot")
typedef struct BlackParrotState BlackParrotState;
DECLARE_INSTANCE_CHECKER(BlackParrotState, BLACKPARROT_MACHINE,
                         TYPE_BLACKPARROT_MACHINE)

struct BlackParrotState {
    /*< private >*/
    MachineState parent;

    /*< public >*/
    RISCVHartArrayState soc[BLACKPARROT_SOCKETS_MAX];
    FWCfgState *fw_cfg;

    int fdt_size;
};

enum {
    BLACKPARROT_DEBUG,
    BLACKPARROT_MROM,
    BLACKPARROT_TEST,
    BLACKPARROT_CLINT,
    BLACKPARROT_UART0,
    BLACKPARROT_FW_CFG,
    BLACKPARROT_FLASH,
    BLACKPARROT_DRAM,
};

#endif
