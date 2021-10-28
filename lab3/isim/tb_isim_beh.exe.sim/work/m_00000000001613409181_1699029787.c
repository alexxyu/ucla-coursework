/**********************************************************************/
/*   ____  ____                                                       */
/*  /   /\/   /                                                       */
/* /___/  \  /                                                        */
/* \   \   \/                                                       */
/*  \   \        Copyright (c) 2003-2009 Xilinx, Inc.                */
/*  /   /          All Right Reserved.                                 */
/* /---/   /\                                                         */
/* \   \  /  \                                                      */
/*  \___\/\___\                                                    */
/***********************************************************************/

/* This file is designed for use with ISim build 0x7708f090 */

#define XSI_HIDE_SYMBOL_SPEC true
#include "xsi.h"
#include <memory.h>
#ifdef __GNUC__
#include <stdlib.h>
#else
#include <malloc.h>
#define alloca _alloca
#endif
static const char *ng0 = "E:/CS152A/lab3/seven_segment_decoder.v";
static int ng1[] = {0, 0};
static unsigned int ng2[] = {63U, 0U};
static int ng3[] = {1, 0};
static unsigned int ng4[] = {6U, 0U};
static int ng5[] = {2, 0};
static unsigned int ng6[] = {91U, 0U};
static int ng7[] = {3, 0};
static unsigned int ng8[] = {79U, 0U};
static int ng9[] = {4, 0};
static unsigned int ng10[] = {102U, 0U};
static int ng11[] = {5, 0};
static unsigned int ng12[] = {109U, 0U};
static int ng13[] = {6, 0};
static unsigned int ng14[] = {125U, 0U};
static int ng15[] = {7, 0};
static unsigned int ng16[] = {7U, 0U};
static int ng17[] = {8, 0};
static unsigned int ng18[] = {127U, 0U};
static int ng19[] = {9, 0};
static unsigned int ng20[] = {111U, 0U};



static void Always_27_0(char *t0)
{
    char t7[8];
    char *t1;
    char *t2;
    char *t3;
    char *t4;
    char *t5;
    int t6;
    char *t8;
    char *t9;
    char *t10;
    unsigned int t11;
    unsigned int t12;
    unsigned int t13;
    unsigned int t14;
    unsigned int t15;
    unsigned int t16;
    unsigned int t17;
    unsigned int t18;
    char *t19;

LAB0:    t1 = (t0 + 2368U);
    t2 = *((char **)t1);
    if (t2 == 0)
        goto LAB2;

LAB3:    goto *t2;

LAB2:    xsi_set_current_line(27, ng0);
    t2 = (t0 + 2688);
    *((int *)t2) = 1;
    t3 = (t0 + 2400);
    *((char **)t3) = t2;
    *((char **)t1) = &&LAB4;

LAB1:    return;
LAB4:    xsi_set_current_line(27, ng0);

LAB5:    xsi_set_current_line(28, ng0);
    t4 = (t0 + 1048U);
    t5 = *((char **)t4);

LAB6:    t4 = ((char*)((ng1)));
    t6 = xsi_vlog_unsigned_case_compare(t5, 4, t4, 32);
    if (t6 == 1)
        goto LAB7;

LAB8:    t2 = ((char*)((ng3)));
    t6 = xsi_vlog_unsigned_case_compare(t5, 4, t2, 32);
    if (t6 == 1)
        goto LAB9;

LAB10:    t2 = ((char*)((ng5)));
    t6 = xsi_vlog_unsigned_case_compare(t5, 4, t2, 32);
    if (t6 == 1)
        goto LAB11;

LAB12:    t2 = ((char*)((ng7)));
    t6 = xsi_vlog_unsigned_case_compare(t5, 4, t2, 32);
    if (t6 == 1)
        goto LAB13;

LAB14:    t2 = ((char*)((ng9)));
    t6 = xsi_vlog_unsigned_case_compare(t5, 4, t2, 32);
    if (t6 == 1)
        goto LAB15;

LAB16:    t2 = ((char*)((ng11)));
    t6 = xsi_vlog_unsigned_case_compare(t5, 4, t2, 32);
    if (t6 == 1)
        goto LAB17;

LAB18:    t2 = ((char*)((ng13)));
    t6 = xsi_vlog_unsigned_case_compare(t5, 4, t2, 32);
    if (t6 == 1)
        goto LAB19;

LAB20:    t2 = ((char*)((ng15)));
    t6 = xsi_vlog_unsigned_case_compare(t5, 4, t2, 32);
    if (t6 == 1)
        goto LAB21;

LAB22:    t2 = ((char*)((ng17)));
    t6 = xsi_vlog_unsigned_case_compare(t5, 4, t2, 32);
    if (t6 == 1)
        goto LAB23;

LAB24:    t2 = ((char*)((ng19)));
    t6 = xsi_vlog_unsigned_case_compare(t5, 4, t2, 32);
    if (t6 == 1)
        goto LAB25;

LAB26:
LAB27:    goto LAB2;

LAB7:    xsi_set_current_line(29, ng0);
    t8 = ((char*)((ng2)));
    memset(t7, 0, 8);
    t9 = (t7 + 4);
    t10 = (t8 + 4);
    t11 = *((unsigned int *)t8);
    t12 = (~(t11));
    *((unsigned int *)t7) = t12;
    *((unsigned int *)t9) = 0;
    if (*((unsigned int *)t10) != 0)
        goto LAB29;

LAB28:    t17 = *((unsigned int *)t7);
    *((unsigned int *)t7) = (t17 & 4294967295U);
    t18 = *((unsigned int *)t9);
    *((unsigned int *)t9) = (t18 & 4294967295U);
    t19 = (t0 + 1448);
    xsi_vlogvar_assign_value(t19, t7, 0, 0, 7);
    goto LAB27;

LAB9:    xsi_set_current_line(30, ng0);
    t3 = ((char*)((ng4)));
    memset(t7, 0, 8);
    t4 = (t7 + 4);
    t8 = (t3 + 4);
    t11 = *((unsigned int *)t3);
    t12 = (~(t11));
    *((unsigned int *)t7) = t12;
    *((unsigned int *)t4) = 0;
    if (*((unsigned int *)t8) != 0)
        goto LAB31;

LAB30:    t17 = *((unsigned int *)t7);
    *((unsigned int *)t7) = (t17 & 4294967295U);
    t18 = *((unsigned int *)t4);
    *((unsigned int *)t4) = (t18 & 4294967295U);
    t9 = (t0 + 1448);
    xsi_vlogvar_assign_value(t9, t7, 0, 0, 7);
    goto LAB27;

LAB11:    xsi_set_current_line(31, ng0);
    t3 = ((char*)((ng6)));
    memset(t7, 0, 8);
    t4 = (t7 + 4);
    t8 = (t3 + 4);
    t11 = *((unsigned int *)t3);
    t12 = (~(t11));
    *((unsigned int *)t7) = t12;
    *((unsigned int *)t4) = 0;
    if (*((unsigned int *)t8) != 0)
        goto LAB33;

LAB32:    t17 = *((unsigned int *)t7);
    *((unsigned int *)t7) = (t17 & 4294967295U);
    t18 = *((unsigned int *)t4);
    *((unsigned int *)t4) = (t18 & 4294967295U);
    t9 = (t0 + 1448);
    xsi_vlogvar_assign_value(t9, t7, 0, 0, 7);
    goto LAB27;

LAB13:    xsi_set_current_line(32, ng0);
    t3 = ((char*)((ng8)));
    memset(t7, 0, 8);
    t4 = (t7 + 4);
    t8 = (t3 + 4);
    t11 = *((unsigned int *)t3);
    t12 = (~(t11));
    *((unsigned int *)t7) = t12;
    *((unsigned int *)t4) = 0;
    if (*((unsigned int *)t8) != 0)
        goto LAB35;

LAB34:    t17 = *((unsigned int *)t7);
    *((unsigned int *)t7) = (t17 & 4294967295U);
    t18 = *((unsigned int *)t4);
    *((unsigned int *)t4) = (t18 & 4294967295U);
    t9 = (t0 + 1448);
    xsi_vlogvar_assign_value(t9, t7, 0, 0, 7);
    goto LAB27;

LAB15:    xsi_set_current_line(33, ng0);
    t3 = ((char*)((ng10)));
    memset(t7, 0, 8);
    t4 = (t7 + 4);
    t8 = (t3 + 4);
    t11 = *((unsigned int *)t3);
    t12 = (~(t11));
    *((unsigned int *)t7) = t12;
    *((unsigned int *)t4) = 0;
    if (*((unsigned int *)t8) != 0)
        goto LAB37;

LAB36:    t17 = *((unsigned int *)t7);
    *((unsigned int *)t7) = (t17 & 4294967295U);
    t18 = *((unsigned int *)t4);
    *((unsigned int *)t4) = (t18 & 4294967295U);
    t9 = (t0 + 1448);
    xsi_vlogvar_assign_value(t9, t7, 0, 0, 7);
    goto LAB27;

LAB17:    xsi_set_current_line(34, ng0);
    t3 = ((char*)((ng12)));
    memset(t7, 0, 8);
    t4 = (t7 + 4);
    t8 = (t3 + 4);
    t11 = *((unsigned int *)t3);
    t12 = (~(t11));
    *((unsigned int *)t7) = t12;
    *((unsigned int *)t4) = 0;
    if (*((unsigned int *)t8) != 0)
        goto LAB39;

LAB38:    t17 = *((unsigned int *)t7);
    *((unsigned int *)t7) = (t17 & 4294967295U);
    t18 = *((unsigned int *)t4);
    *((unsigned int *)t4) = (t18 & 4294967295U);
    t9 = (t0 + 1448);
    xsi_vlogvar_assign_value(t9, t7, 0, 0, 7);
    goto LAB27;

LAB19:    xsi_set_current_line(35, ng0);
    t3 = ((char*)((ng14)));
    memset(t7, 0, 8);
    t4 = (t7 + 4);
    t8 = (t3 + 4);
    t11 = *((unsigned int *)t3);
    t12 = (~(t11));
    *((unsigned int *)t7) = t12;
    *((unsigned int *)t4) = 0;
    if (*((unsigned int *)t8) != 0)
        goto LAB41;

LAB40:    t17 = *((unsigned int *)t7);
    *((unsigned int *)t7) = (t17 & 4294967295U);
    t18 = *((unsigned int *)t4);
    *((unsigned int *)t4) = (t18 & 4294967295U);
    t9 = (t0 + 1448);
    xsi_vlogvar_assign_value(t9, t7, 0, 0, 7);
    goto LAB27;

LAB21:    xsi_set_current_line(36, ng0);
    t3 = ((char*)((ng16)));
    memset(t7, 0, 8);
    t4 = (t7 + 4);
    t8 = (t3 + 4);
    t11 = *((unsigned int *)t3);
    t12 = (~(t11));
    *((unsigned int *)t7) = t12;
    *((unsigned int *)t4) = 0;
    if (*((unsigned int *)t8) != 0)
        goto LAB43;

LAB42:    t17 = *((unsigned int *)t7);
    *((unsigned int *)t7) = (t17 & 4294967295U);
    t18 = *((unsigned int *)t4);
    *((unsigned int *)t4) = (t18 & 4294967295U);
    t9 = (t0 + 1448);
    xsi_vlogvar_assign_value(t9, t7, 0, 0, 7);
    goto LAB27;

LAB23:    xsi_set_current_line(37, ng0);
    t3 = ((char*)((ng18)));
    memset(t7, 0, 8);
    t4 = (t7 + 4);
    t8 = (t3 + 4);
    t11 = *((unsigned int *)t3);
    t12 = (~(t11));
    *((unsigned int *)t7) = t12;
    *((unsigned int *)t4) = 0;
    if (*((unsigned int *)t8) != 0)
        goto LAB45;

LAB44:    t17 = *((unsigned int *)t7);
    *((unsigned int *)t7) = (t17 & 4294967295U);
    t18 = *((unsigned int *)t4);
    *((unsigned int *)t4) = (t18 & 4294967295U);
    t9 = (t0 + 1448);
    xsi_vlogvar_assign_value(t9, t7, 0, 0, 7);
    goto LAB27;

LAB25:    xsi_set_current_line(38, ng0);
    t3 = ((char*)((ng20)));
    memset(t7, 0, 8);
    t4 = (t7 + 4);
    t8 = (t3 + 4);
    t11 = *((unsigned int *)t3);
    t12 = (~(t11));
    *((unsigned int *)t7) = t12;
    *((unsigned int *)t4) = 0;
    if (*((unsigned int *)t8) != 0)
        goto LAB47;

LAB46:    t17 = *((unsigned int *)t7);
    *((unsigned int *)t7) = (t17 & 4294967295U);
    t18 = *((unsigned int *)t4);
    *((unsigned int *)t4) = (t18 & 4294967295U);
    t9 = (t0 + 1448);
    xsi_vlogvar_assign_value(t9, t7, 0, 0, 7);
    goto LAB27;

LAB29:    t13 = *((unsigned int *)t7);
    t14 = *((unsigned int *)t10);
    *((unsigned int *)t7) = (t13 | t14);
    t15 = *((unsigned int *)t9);
    t16 = *((unsigned int *)t10);
    *((unsigned int *)t9) = (t15 | t16);
    goto LAB28;

LAB31:    t13 = *((unsigned int *)t7);
    t14 = *((unsigned int *)t8);
    *((unsigned int *)t7) = (t13 | t14);
    t15 = *((unsigned int *)t4);
    t16 = *((unsigned int *)t8);
    *((unsigned int *)t4) = (t15 | t16);
    goto LAB30;

LAB33:    t13 = *((unsigned int *)t7);
    t14 = *((unsigned int *)t8);
    *((unsigned int *)t7) = (t13 | t14);
    t15 = *((unsigned int *)t4);
    t16 = *((unsigned int *)t8);
    *((unsigned int *)t4) = (t15 | t16);
    goto LAB32;

LAB35:    t13 = *((unsigned int *)t7);
    t14 = *((unsigned int *)t8);
    *((unsigned int *)t7) = (t13 | t14);
    t15 = *((unsigned int *)t4);
    t16 = *((unsigned int *)t8);
    *((unsigned int *)t4) = (t15 | t16);
    goto LAB34;

LAB37:    t13 = *((unsigned int *)t7);
    t14 = *((unsigned int *)t8);
    *((unsigned int *)t7) = (t13 | t14);
    t15 = *((unsigned int *)t4);
    t16 = *((unsigned int *)t8);
    *((unsigned int *)t4) = (t15 | t16);
    goto LAB36;

LAB39:    t13 = *((unsigned int *)t7);
    t14 = *((unsigned int *)t8);
    *((unsigned int *)t7) = (t13 | t14);
    t15 = *((unsigned int *)t4);
    t16 = *((unsigned int *)t8);
    *((unsigned int *)t4) = (t15 | t16);
    goto LAB38;

LAB41:    t13 = *((unsigned int *)t7);
    t14 = *((unsigned int *)t8);
    *((unsigned int *)t7) = (t13 | t14);
    t15 = *((unsigned int *)t4);
    t16 = *((unsigned int *)t8);
    *((unsigned int *)t4) = (t15 | t16);
    goto LAB40;

LAB43:    t13 = *((unsigned int *)t7);
    t14 = *((unsigned int *)t8);
    *((unsigned int *)t7) = (t13 | t14);
    t15 = *((unsigned int *)t4);
    t16 = *((unsigned int *)t8);
    *((unsigned int *)t4) = (t15 | t16);
    goto LAB42;

LAB45:    t13 = *((unsigned int *)t7);
    t14 = *((unsigned int *)t8);
    *((unsigned int *)t7) = (t13 | t14);
    t15 = *((unsigned int *)t4);
    t16 = *((unsigned int *)t8);
    *((unsigned int *)t4) = (t15 | t16);
    goto LAB44;

LAB47:    t13 = *((unsigned int *)t7);
    t14 = *((unsigned int *)t8);
    *((unsigned int *)t7) = (t13 | t14);
    t15 = *((unsigned int *)t4);
    t16 = *((unsigned int *)t8);
    *((unsigned int *)t4) = (t15 | t16);
    goto LAB46;

}


extern void work_m_00000000001613409181_1699029787_init()
{
	static char *pe[] = {(void *)Always_27_0};
	xsi_register_didat("work_m_00000000001613409181_1699029787", "isim/tb_isim_beh.exe.sim/work/m_00000000001613409181_1699029787.didat");
	xsi_register_executes(pe);
}
