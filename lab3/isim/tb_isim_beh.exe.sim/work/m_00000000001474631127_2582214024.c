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
static const char *ng0 = "E:/CS152A/lab3/counter.v";
static int ng1[] = {0, 0};
static int ng2[] = {59, 0};
static int ng3[] = {1, 0};



static void Always_29_0(char *t0)
{
    char t13[8];
    char t21[8];
    char t35[8];
    char *t1;
    char *t2;
    char *t3;
    char *t4;
    char *t5;
    unsigned int t6;
    unsigned int t7;
    unsigned int t8;
    unsigned int t9;
    unsigned int t10;
    char *t11;
    char *t12;
    unsigned int t14;
    unsigned int t15;
    unsigned int t16;
    unsigned int t17;
    unsigned int t18;
    char *t19;
    char *t20;
    char *t22;
    char *t23;
    char *t24;
    char *t25;
    unsigned int t26;
    unsigned int t27;
    unsigned int t28;
    unsigned int t29;
    unsigned int t30;
    char *t31;
    char *t32;
    char *t33;
    char *t34;
    char *t36;

LAB0:    t1 = (t0 + 2848U);
    t2 = *((char **)t1);
    if (t2 == 0)
        goto LAB2;

LAB3:    goto *t2;

LAB2:    xsi_set_current_line(29, ng0);
    t2 = (t0 + 3168);
    *((int *)t2) = 1;
    t3 = (t0 + 2880);
    *((char **)t3) = t2;
    *((char **)t1) = &&LAB4;

LAB1:    return;
LAB4:    xsi_set_current_line(29, ng0);

LAB5:    xsi_set_current_line(30, ng0);
    t4 = (t0 + 1208U);
    t5 = *((char **)t4);
    t4 = (t5 + 4);
    t6 = *((unsigned int *)t4);
    t7 = (~(t6));
    t8 = *((unsigned int *)t5);
    t9 = (t8 & t7);
    t10 = (t9 != 0);
    if (t10 > 0)
        goto LAB6;

LAB7:    xsi_set_current_line(33, ng0);
    t2 = (t0 + 1368U);
    t3 = *((char **)t2);
    memset(t13, 0, 8);
    t2 = (t3 + 4);
    t6 = *((unsigned int *)t2);
    t7 = (~(t6));
    t8 = *((unsigned int *)t3);
    t9 = (t8 & t7);
    t10 = (t9 & 1U);
    if (t10 != 0)
        goto LAB13;

LAB11:    if (*((unsigned int *)t2) == 0)
        goto LAB10;

LAB12:    t4 = (t13 + 4);
    *((unsigned int *)t13) = 1;
    *((unsigned int *)t4) = 1;

LAB13:    t5 = (t13 + 4);
    t14 = *((unsigned int *)t5);
    t15 = (~(t14));
    t16 = *((unsigned int *)t13);
    t17 = (t16 & t15);
    t18 = (t17 != 0);
    if (t18 > 0)
        goto LAB14;

LAB15:
LAB16:
LAB8:    goto LAB2;

LAB6:    xsi_set_current_line(30, ng0);

LAB9:    xsi_set_current_line(31, ng0);
    t11 = ((char*)((ng1)));
    t12 = (t0 + 1768);
    xsi_vlogvar_wait_assign_value(t12, t11, 0, 0, 6, 0LL);
    xsi_set_current_line(32, ng0);
    t2 = ((char*)((ng1)));
    t3 = (t0 + 1928);
    xsi_vlogvar_wait_assign_value(t3, t2, 0, 0, 6, 0LL);
    goto LAB8;

LAB10:    *((unsigned int *)t13) = 1;
    goto LAB13;

LAB14:    xsi_set_current_line(33, ng0);

LAB17:    xsi_set_current_line(34, ng0);
    t11 = (t0 + 1928);
    t12 = (t11 + 56U);
    t19 = *((char **)t12);
    t20 = ((char*)((ng2)));
    memset(t21, 0, 8);
    t22 = (t19 + 4);
    if (*((unsigned int *)t22) != 0)
        goto LAB19;

LAB18:    t23 = (t20 + 4);
    if (*((unsigned int *)t23) != 0)
        goto LAB19;

LAB22:    if (*((unsigned int *)t19) < *((unsigned int *)t20))
        goto LAB20;

LAB21:    t25 = (t21 + 4);
    t26 = *((unsigned int *)t25);
    t27 = (~(t26));
    t28 = *((unsigned int *)t21);
    t29 = (t28 & t27);
    t30 = (t29 != 0);
    if (t30 > 0)
        goto LAB23;

LAB24:    xsi_set_current_line(36, ng0);

LAB27:    xsi_set_current_line(37, ng0);
    t2 = ((char*)((ng1)));
    t3 = (t0 + 1928);
    xsi_vlogvar_wait_assign_value(t3, t2, 0, 0, 6, 0LL);
    xsi_set_current_line(38, ng0);
    t2 = (t0 + 1768);
    t3 = (t2 + 56U);
    t4 = *((char **)t3);
    t5 = ((char*)((ng2)));
    memset(t13, 0, 8);
    t11 = (t4 + 4);
    if (*((unsigned int *)t11) != 0)
        goto LAB29;

LAB28:    t12 = (t5 + 4);
    if (*((unsigned int *)t12) != 0)
        goto LAB29;

LAB32:    if (*((unsigned int *)t4) < *((unsigned int *)t5))
        goto LAB30;

LAB31:    t20 = (t13 + 4);
    t6 = *((unsigned int *)t20);
    t7 = (~(t6));
    t8 = *((unsigned int *)t13);
    t9 = (t8 & t7);
    t10 = (t9 != 0);
    if (t10 > 0)
        goto LAB33;

LAB34:    xsi_set_current_line(40, ng0);

LAB37:    xsi_set_current_line(41, ng0);
    t2 = ((char*)((ng1)));
    t3 = (t0 + 1768);
    xsi_vlogvar_wait_assign_value(t3, t2, 0, 0, 6, 0LL);

LAB35:
LAB25:    goto LAB16;

LAB19:    t24 = (t21 + 4);
    *((unsigned int *)t21) = 1;
    *((unsigned int *)t24) = 1;
    goto LAB21;

LAB20:    *((unsigned int *)t21) = 1;
    goto LAB21;

LAB23:    xsi_set_current_line(34, ng0);

LAB26:    xsi_set_current_line(35, ng0);
    t31 = (t0 + 1928);
    t32 = (t31 + 56U);
    t33 = *((char **)t32);
    t34 = ((char*)((ng3)));
    memset(t35, 0, 8);
    xsi_vlog_unsigned_add(t35, 32, t33, 6, t34, 32);
    t36 = (t0 + 1928);
    xsi_vlogvar_wait_assign_value(t36, t35, 0, 0, 6, 0LL);
    goto LAB25;

LAB29:    t19 = (t13 + 4);
    *((unsigned int *)t13) = 1;
    *((unsigned int *)t19) = 1;
    goto LAB31;

LAB30:    *((unsigned int *)t13) = 1;
    goto LAB31;

LAB33:    xsi_set_current_line(38, ng0);

LAB36:    xsi_set_current_line(39, ng0);
    t22 = (t0 + 1768);
    t23 = (t22 + 56U);
    t24 = *((char **)t23);
    t25 = ((char*)((ng3)));
    memset(t21, 0, 8);
    xsi_vlog_unsigned_add(t21, 32, t24, 6, t25, 32);
    t31 = (t0 + 1768);
    xsi_vlogvar_wait_assign_value(t31, t21, 0, 0, 6, 0LL);
    goto LAB35;

}


extern void work_m_00000000001474631127_2582214024_init()
{
	static char *pe[] = {(void *)Always_29_0};
	xsi_register_didat("work_m_00000000001474631127_2582214024", "isim/tb_isim_beh.exe.sim/work/m_00000000001474631127_2582214024.didat");
	xsi_register_executes(pe);
}
