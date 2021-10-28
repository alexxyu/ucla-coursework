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
static const char *ng0 = "E:/CS152A/lab3/state_machine.v";
static int ng1[] = {1, 0};
static int ng2[] = {0, 0};



static void Cont_38_0(char *t0)
{
    char t7[8];
    char *t1;
    char *t2;
    char *t3;
    char *t4;
    char *t5;
    char *t6;
    char *t8;
    unsigned int t9;
    unsigned int t10;
    unsigned int t11;
    unsigned int t12;
    unsigned int t13;
    unsigned int t14;
    unsigned int t15;
    unsigned int t16;
    unsigned int t17;
    unsigned int t18;
    unsigned int t19;
    unsigned int t20;
    char *t21;
    char *t22;
    char *t23;
    char *t24;
    char *t25;
    char *t26;
    unsigned int t27;
    unsigned int t28;
    char *t29;
    unsigned int t30;
    unsigned int t31;
    char *t32;
    unsigned int t33;
    unsigned int t34;
    char *t35;

LAB0:    t1 = (t0 + 3872U);
    t2 = *((char **)t1);
    if (t2 == 0)
        goto LAB2;

LAB3:    goto *t2;

LAB2:    xsi_set_current_line(38, ng0);
    t2 = (t0 + 2792);
    t3 = (t2 + 56U);
    t4 = *((char **)t3);
    t5 = (t0 + 608);
    t6 = *((char **)t5);
    memset(t7, 0, 8);
    t5 = (t4 + 4);
    t8 = (t6 + 4);
    t9 = *((unsigned int *)t4);
    t10 = *((unsigned int *)t6);
    t11 = (t9 ^ t10);
    t12 = *((unsigned int *)t5);
    t13 = *((unsigned int *)t8);
    t14 = (t12 ^ t13);
    t15 = (t11 | t14);
    t16 = *((unsigned int *)t5);
    t17 = *((unsigned int *)t8);
    t18 = (t16 | t17);
    t19 = (~(t18));
    t20 = (t15 & t19);
    if (t20 != 0)
        goto LAB7;

LAB4:    if (t18 != 0)
        goto LAB6;

LAB5:    *((unsigned int *)t7) = 1;

LAB7:    t22 = (t0 + 4800);
    t23 = (t22 + 56U);
    t24 = *((char **)t23);
    t25 = (t24 + 56U);
    t26 = *((char **)t25);
    memset(t26, 0, 8);
    t27 = 1U;
    t28 = t27;
    t29 = (t7 + 4);
    t30 = *((unsigned int *)t7);
    t27 = (t27 & t30);
    t31 = *((unsigned int *)t29);
    t28 = (t28 & t31);
    t32 = (t26 + 4);
    t33 = *((unsigned int *)t26);
    *((unsigned int *)t26) = (t33 | t27);
    t34 = *((unsigned int *)t32);
    *((unsigned int *)t32) = (t34 | t28);
    xsi_driver_vfirst_trans(t22, 0, 0);
    t35 = (t0 + 4688);
    *((int *)t35) = 1;

LAB1:    return;
LAB6:    t21 = (t7 + 4);
    *((unsigned int *)t7) = 1;
    *((unsigned int *)t21) = 1;
    goto LAB7;

}

static void Always_40_1(char *t0)
{
    char t18[8];
    char *t1;
    char *t2;
    char *t3;
    char *t4;
    char *t5;
    char *t6;
    char *t7;
    char *t8;
    int t9;
    char *t10;
    unsigned int t11;
    unsigned int t12;
    unsigned int t13;
    unsigned int t14;
    unsigned int t15;
    char *t16;
    char *t17;
    unsigned int t19;
    unsigned int t20;
    unsigned int t21;
    unsigned int t22;
    unsigned int t23;
    unsigned int t24;
    unsigned int t25;
    unsigned int t26;
    unsigned int t27;
    unsigned int t28;
    unsigned int t29;
    unsigned int t30;
    unsigned int t31;
    unsigned int t32;
    unsigned int t33;
    unsigned int t34;
    unsigned int t35;
    char *t36;

LAB0:    t1 = (t0 + 4120U);
    t2 = *((char **)t1);
    if (t2 == 0)
        goto LAB2;

LAB3:    goto *t2;

LAB2:    xsi_set_current_line(40, ng0);
    t2 = (t0 + 4704);
    *((int *)t2) = 1;
    t3 = (t0 + 4152);
    *((char **)t3) = t2;
    *((char **)t1) = &&LAB4;

LAB1:    return;
LAB4:    xsi_set_current_line(40, ng0);

LAB5:    xsi_set_current_line(41, ng0);
    t4 = (t0 + 2792);
    t5 = (t4 + 56U);
    t6 = *((char **)t5);

LAB6:    t7 = (t0 + 472);
    t8 = *((char **)t7);
    t9 = xsi_vlog_unsigned_case_compare(t6, 2, t8, 32);
    if (t9 == 1)
        goto LAB7;

LAB8:    t2 = (t0 + 608);
    t3 = *((char **)t2);
    t9 = xsi_vlog_unsigned_case_compare(t6, 2, t3, 32);
    if (t9 == 1)
        goto LAB9;

LAB10:    t2 = (t0 + 744);
    t3 = *((char **)t2);
    t9 = xsi_vlog_unsigned_case_compare(t6, 2, t3, 32);
    if (t9 == 1)
        goto LAB11;

LAB12:    t2 = (t0 + 880);
    t3 = *((char **)t2);
    t9 = xsi_vlog_unsigned_case_compare(t6, 2, t3, 32);
    if (t9 == 1)
        goto LAB13;

LAB14:
LAB15:    goto LAB2;

LAB7:    xsi_set_current_line(42, ng0);

LAB16:    xsi_set_current_line(43, ng0);
    t7 = (t0 + 2232U);
    t10 = *((char **)t7);
    t7 = (t10 + 4);
    t11 = *((unsigned int *)t7);
    t12 = (~(t11));
    t13 = *((unsigned int *)t10);
    t14 = (t13 & t12);
    t15 = (t14 != 0);
    if (t15 > 0)
        goto LAB17;

LAB18:    xsi_set_current_line(45, ng0);
    t2 = (t0 + 1912U);
    t3 = *((char **)t2);
    t2 = (t3 + 4);
    t11 = *((unsigned int *)t2);
    t12 = (~(t11));
    t13 = *((unsigned int *)t3);
    t14 = (t13 & t12);
    t15 = (t14 != 0);
    if (t15 > 0)
        goto LAB21;

LAB22:
LAB23:
LAB19:    goto LAB15;

LAB9:    xsi_set_current_line(53, ng0);

LAB34:    xsi_set_current_line(54, ng0);
    t2 = (t0 + 2232U);
    t4 = *((char **)t2);
    t2 = (t4 + 4);
    t11 = *((unsigned int *)t2);
    t12 = (~(t11));
    t13 = *((unsigned int *)t4);
    t14 = (t13 & t12);
    t15 = (t14 != 0);
    if (t15 > 0)
        goto LAB35;

LAB36:
LAB37:    goto LAB15;

LAB11:    xsi_set_current_line(58, ng0);

LAB39:    xsi_set_current_line(59, ng0);
    t2 = (t0 + 1912U);
    t4 = *((char **)t2);
    t2 = ((char*)((ng2)));
    memset(t18, 0, 8);
    t5 = (t4 + 4);
    t7 = (t2 + 4);
    t11 = *((unsigned int *)t4);
    t12 = *((unsigned int *)t2);
    t13 = (t11 ^ t12);
    t14 = *((unsigned int *)t5);
    t15 = *((unsigned int *)t7);
    t19 = (t14 ^ t15);
    t20 = (t13 | t19);
    t21 = *((unsigned int *)t5);
    t22 = *((unsigned int *)t7);
    t23 = (t21 | t22);
    t24 = (~(t23));
    t25 = (t20 & t24);
    if (t25 != 0)
        goto LAB43;

LAB40:    if (t23 != 0)
        goto LAB42;

LAB41:    *((unsigned int *)t18) = 1;

LAB43:    t10 = (t18 + 4);
    t26 = *((unsigned int *)t10);
    t27 = (~(t26));
    t28 = *((unsigned int *)t18);
    t29 = (t28 & t27);
    t30 = (t29 != 0);
    if (t30 > 0)
        goto LAB44;

LAB45:    xsi_set_current_line(61, ng0);
    t2 = (t0 + 2072U);
    t3 = *((char **)t2);
    t2 = ((char*)((ng2)));
    memset(t18, 0, 8);
    t4 = (t3 + 4);
    t5 = (t2 + 4);
    t11 = *((unsigned int *)t3);
    t12 = *((unsigned int *)t2);
    t13 = (t11 ^ t12);
    t14 = *((unsigned int *)t4);
    t15 = *((unsigned int *)t5);
    t19 = (t14 ^ t15);
    t20 = (t13 | t19);
    t21 = *((unsigned int *)t4);
    t22 = *((unsigned int *)t5);
    t23 = (t21 | t22);
    t24 = (~(t23));
    t25 = (t20 & t24);
    if (t25 != 0)
        goto LAB51;

LAB48:    if (t23 != 0)
        goto LAB50;

LAB49:    *((unsigned int *)t18) = 1;

LAB51:    t8 = (t18 + 4);
    t26 = *((unsigned int *)t8);
    t27 = (~(t26));
    t28 = *((unsigned int *)t18);
    t29 = (t28 & t27);
    t30 = (t29 != 0);
    if (t30 > 0)
        goto LAB52;

LAB53:
LAB54:
LAB46:    goto LAB15;

LAB13:    xsi_set_current_line(65, ng0);

LAB56:    xsi_set_current_line(66, ng0);
    t2 = (t0 + 1912U);
    t4 = *((char **)t2);
    t2 = ((char*)((ng2)));
    memset(t18, 0, 8);
    t5 = (t4 + 4);
    t7 = (t2 + 4);
    t11 = *((unsigned int *)t4);
    t12 = *((unsigned int *)t2);
    t13 = (t11 ^ t12);
    t14 = *((unsigned int *)t5);
    t15 = *((unsigned int *)t7);
    t19 = (t14 ^ t15);
    t20 = (t13 | t19);
    t21 = *((unsigned int *)t5);
    t22 = *((unsigned int *)t7);
    t23 = (t21 | t22);
    t24 = (~(t23));
    t25 = (t20 & t24);
    if (t25 != 0)
        goto LAB60;

LAB57:    if (t23 != 0)
        goto LAB59;

LAB58:    *((unsigned int *)t18) = 1;

LAB60:    t10 = (t18 + 4);
    t26 = *((unsigned int *)t10);
    t27 = (~(t26));
    t28 = *((unsigned int *)t18);
    t29 = (t28 & t27);
    t30 = (t29 != 0);
    if (t30 > 0)
        goto LAB61;

LAB62:    xsi_set_current_line(68, ng0);
    t2 = (t0 + 2072U);
    t3 = *((char **)t2);
    t2 = ((char*)((ng1)));
    memset(t18, 0, 8);
    t4 = (t3 + 4);
    t5 = (t2 + 4);
    t11 = *((unsigned int *)t3);
    t12 = *((unsigned int *)t2);
    t13 = (t11 ^ t12);
    t14 = *((unsigned int *)t4);
    t15 = *((unsigned int *)t5);
    t19 = (t14 ^ t15);
    t20 = (t13 | t19);
    t21 = *((unsigned int *)t4);
    t22 = *((unsigned int *)t5);
    t23 = (t21 | t22);
    t24 = (~(t23));
    t25 = (t20 & t24);
    if (t25 != 0)
        goto LAB68;

LAB65:    if (t23 != 0)
        goto LAB67;

LAB66:    *((unsigned int *)t18) = 1;

LAB68:    t8 = (t18 + 4);
    t26 = *((unsigned int *)t8);
    t27 = (~(t26));
    t28 = *((unsigned int *)t18);
    t29 = (t28 & t27);
    t30 = (t29 != 0);
    if (t30 > 0)
        goto LAB69;

LAB70:
LAB71:
LAB63:    goto LAB15;

LAB17:    xsi_set_current_line(43, ng0);

LAB20:    xsi_set_current_line(44, ng0);
    t16 = (t0 + 608);
    t17 = *((char **)t16);
    t16 = (t0 + 2952);
    xsi_vlogvar_assign_value(t16, t17, 0, 0, 2);
    goto LAB19;

LAB21:    xsi_set_current_line(45, ng0);

LAB24:    xsi_set_current_line(46, ng0);
    t4 = (t0 + 2072U);
    t5 = *((char **)t4);
    t4 = ((char*)((ng1)));
    memset(t18, 0, 8);
    t7 = (t5 + 4);
    t8 = (t4 + 4);
    t19 = *((unsigned int *)t5);
    t20 = *((unsigned int *)t4);
    t21 = (t19 ^ t20);
    t22 = *((unsigned int *)t7);
    t23 = *((unsigned int *)t8);
    t24 = (t22 ^ t23);
    t25 = (t21 | t24);
    t26 = *((unsigned int *)t7);
    t27 = *((unsigned int *)t8);
    t28 = (t26 | t27);
    t29 = (~(t28));
    t30 = (t25 & t29);
    if (t30 != 0)
        goto LAB28;

LAB25:    if (t28 != 0)
        goto LAB27;

LAB26:    *((unsigned int *)t18) = 1;

LAB28:    t16 = (t18 + 4);
    t31 = *((unsigned int *)t16);
    t32 = (~(t31));
    t33 = *((unsigned int *)t18);
    t34 = (t33 & t32);
    t35 = (t34 != 0);
    if (t35 > 0)
        goto LAB29;

LAB30:    xsi_set_current_line(48, ng0);

LAB33:    xsi_set_current_line(49, ng0);
    t2 = (t0 + 880);
    t3 = *((char **)t2);
    t2 = (t0 + 2952);
    xsi_vlogvar_assign_value(t2, t3, 0, 0, 2);

LAB31:    goto LAB23;

LAB27:    t10 = (t18 + 4);
    *((unsigned int *)t18) = 1;
    *((unsigned int *)t10) = 1;
    goto LAB28;

LAB29:    xsi_set_current_line(46, ng0);

LAB32:    xsi_set_current_line(47, ng0);
    t17 = (t0 + 744);
    t36 = *((char **)t17);
    t17 = (t0 + 2952);
    xsi_vlogvar_assign_value(t17, t36, 0, 0, 2);
    goto LAB31;

LAB35:    xsi_set_current_line(54, ng0);

LAB38:    xsi_set_current_line(55, ng0);
    t5 = (t0 + 472);
    t7 = *((char **)t5);
    t5 = (t0 + 2952);
    xsi_vlogvar_assign_value(t5, t7, 0, 0, 2);
    goto LAB37;

LAB42:    t8 = (t18 + 4);
    *((unsigned int *)t18) = 1;
    *((unsigned int *)t8) = 1;
    goto LAB43;

LAB44:    xsi_set_current_line(59, ng0);

LAB47:    xsi_set_current_line(60, ng0);
    t16 = (t0 + 472);
    t17 = *((char **)t16);
    t16 = (t0 + 2952);
    xsi_vlogvar_assign_value(t16, t17, 0, 0, 2);
    goto LAB46;

LAB50:    t7 = (t18 + 4);
    *((unsigned int *)t18) = 1;
    *((unsigned int *)t7) = 1;
    goto LAB51;

LAB52:    xsi_set_current_line(61, ng0);

LAB55:    xsi_set_current_line(62, ng0);
    t10 = (t0 + 880);
    t16 = *((char **)t10);
    t10 = (t0 + 2952);
    xsi_vlogvar_assign_value(t10, t16, 0, 0, 2);
    goto LAB54;

LAB59:    t8 = (t18 + 4);
    *((unsigned int *)t18) = 1;
    *((unsigned int *)t8) = 1;
    goto LAB60;

LAB61:    xsi_set_current_line(66, ng0);

LAB64:    xsi_set_current_line(67, ng0);
    t16 = (t0 + 472);
    t17 = *((char **)t16);
    t16 = (t0 + 2952);
    xsi_vlogvar_assign_value(t16, t17, 0, 0, 2);
    goto LAB63;

LAB67:    t7 = (t18 + 4);
    *((unsigned int *)t18) = 1;
    *((unsigned int *)t7) = 1;
    goto LAB68;

LAB69:    xsi_set_current_line(68, ng0);

LAB72:    xsi_set_current_line(69, ng0);
    t10 = (t0 + 744);
    t16 = *((char **)t10);
    t10 = (t0 + 2952);
    xsi_vlogvar_assign_value(t10, t16, 0, 0, 2);
    goto LAB71;

}

static void Always_75_2(char *t0)
{
    char *t1;
    char *t2;
    char *t3;
    char *t4;
    char *t5;
    char *t6;
    char *t7;

LAB0:    t1 = (t0 + 4368U);
    t2 = *((char **)t1);
    if (t2 == 0)
        goto LAB2;

LAB3:    goto *t2;

LAB2:    xsi_set_current_line(75, ng0);
    t2 = (t0 + 4720);
    *((int *)t2) = 1;
    t3 = (t0 + 4400);
    *((char **)t3) = t2;
    *((char **)t1) = &&LAB4;

LAB1:    return;
LAB4:    xsi_set_current_line(75, ng0);

LAB5:    xsi_set_current_line(76, ng0);
    t4 = (t0 + 2952);
    t5 = (t4 + 56U);
    t6 = *((char **)t5);
    t7 = (t0 + 2792);
    xsi_vlogvar_wait_assign_value(t7, t6, 0, 0, 2, 0LL);
    goto LAB2;

}


extern void work_m_00000000000236853039_0457016286_init()
{
	static char *pe[] = {(void *)Cont_38_0,(void *)Always_40_1,(void *)Always_75_2};
	xsi_register_didat("work_m_00000000000236853039_0457016286", "isim/tb_isim_beh.exe.sim/work/m_00000000000236853039_0457016286.didat");
	xsi_register_executes(pe);
}
