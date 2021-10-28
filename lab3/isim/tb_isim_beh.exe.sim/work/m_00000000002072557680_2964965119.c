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
static const char *ng0 = "E:/CS152A/lab3/clock.v";
static int ng1[] = {50000000, 0};
static int ng2[] = {0, 0};
static int ng3[] = {1, 0};
static int ng4[] = {25000000, 0};
static int ng5[] = {50000, 0};
static int ng6[] = {17500000, 0};



static void Always_33_0(char *t0)
{
    char t8[8];
    char t15[8];
    char *t1;
    char *t2;
    char *t3;
    char *t4;
    char *t5;
    char *t6;
    char *t7;
    char *t9;
    unsigned int t10;
    unsigned int t11;
    unsigned int t12;
    unsigned int t13;
    unsigned int t14;
    char *t16;
    char *t17;
    char *t18;
    char *t19;
    unsigned int t20;
    unsigned int t21;
    unsigned int t22;
    unsigned int t23;
    unsigned int t24;
    char *t25;
    char *t26;
    char *t27;
    unsigned int t28;
    unsigned int t29;
    unsigned int t30;
    unsigned int t31;
    unsigned int t32;
    unsigned int t33;
    unsigned int t34;
    unsigned int t35;
    char *t36;

LAB0:    t1 = (t0 + 3488U);
    t2 = *((char **)t1);
    if (t2 == 0)
        goto LAB2;

LAB3:    goto *t2;

LAB2:    xsi_set_current_line(33, ng0);
    t2 = (t0 + 3808);
    *((int *)t2) = 1;
    t3 = (t0 + 3520);
    *((char **)t3) = t2;
    *((char **)t1) = &&LAB4;

LAB1:    return;
LAB4:    xsi_set_current_line(33, ng0);

LAB5:    xsi_set_current_line(34, ng0);
    t4 = (t0 + 2088);
    t5 = (t4 + 56U);
    t6 = *((char **)t5);
    t7 = ((char*)((ng1)));
    memset(t8, 0, 8);
    xsi_vlog_signed_equal(t8, 32, t6, 32, t7, 32);
    t9 = (t8 + 4);
    t10 = *((unsigned int *)t9);
    t11 = (~(t10));
    t12 = *((unsigned int *)t8);
    t13 = (t12 & t11);
    t14 = (t13 != 0);
    if (t14 > 0)
        goto LAB6;

LAB7:    xsi_set_current_line(38, ng0);
    t2 = (t0 + 2088);
    t3 = (t2 + 56U);
    t4 = *((char **)t3);
    t5 = ((char*)((ng3)));
    memset(t8, 0, 8);
    xsi_vlog_signed_add(t8, 32, t4, 32, t5, 32);
    t6 = (t0 + 2088);
    xsi_vlogvar_wait_assign_value(t6, t8, 0, 0, 32, 0LL);

LAB8:    xsi_set_current_line(40, ng0);
    t2 = (t0 + 2248);
    t3 = (t2 + 56U);
    t4 = *((char **)t3);
    t5 = ((char*)((ng4)));
    memset(t8, 0, 8);
    xsi_vlog_signed_equal(t8, 32, t4, 32, t5, 32);
    t6 = (t8 + 4);
    t10 = *((unsigned int *)t6);
    t11 = (~(t10));
    t12 = *((unsigned int *)t8);
    t13 = (t12 & t11);
    t14 = (t13 != 0);
    if (t14 > 0)
        goto LAB16;

LAB17:    xsi_set_current_line(44, ng0);
    t2 = (t0 + 2248);
    t3 = (t2 + 56U);
    t4 = *((char **)t3);
    t5 = ((char*)((ng3)));
    memset(t8, 0, 8);
    xsi_vlog_signed_add(t8, 32, t4, 32, t5, 32);
    t6 = (t0 + 2248);
    xsi_vlogvar_wait_assign_value(t6, t8, 0, 0, 32, 0LL);

LAB18:    xsi_set_current_line(46, ng0);
    t2 = (t0 + 2408);
    t3 = (t2 + 56U);
    t4 = *((char **)t3);
    t5 = ((char*)((ng5)));
    memset(t8, 0, 8);
    xsi_vlog_signed_equal(t8, 32, t4, 32, t5, 32);
    t6 = (t8 + 4);
    t10 = *((unsigned int *)t6);
    t11 = (~(t10));
    t12 = *((unsigned int *)t8);
    t13 = (t12 & t11);
    t14 = (t13 != 0);
    if (t14 > 0)
        goto LAB26;

LAB27:    xsi_set_current_line(50, ng0);
    t2 = (t0 + 2408);
    t3 = (t2 + 56U);
    t4 = *((char **)t3);
    t5 = ((char*)((ng3)));
    memset(t8, 0, 8);
    xsi_vlog_signed_add(t8, 32, t4, 32, t5, 32);
    t6 = (t0 + 2408);
    xsi_vlogvar_wait_assign_value(t6, t8, 0, 0, 32, 0LL);

LAB28:    xsi_set_current_line(52, ng0);
    t2 = (t0 + 2568);
    t3 = (t2 + 56U);
    t4 = *((char **)t3);
    t5 = ((char*)((ng6)));
    memset(t8, 0, 8);
    xsi_vlog_signed_equal(t8, 32, t4, 32, t5, 32);
    t6 = (t8 + 4);
    t10 = *((unsigned int *)t6);
    t11 = (~(t10));
    t12 = *((unsigned int *)t8);
    t13 = (t12 & t11);
    t14 = (t13 != 0);
    if (t14 > 0)
        goto LAB36;

LAB37:    xsi_set_current_line(56, ng0);
    t2 = (t0 + 2568);
    t3 = (t2 + 56U);
    t4 = *((char **)t3);
    t5 = ((char*)((ng3)));
    memset(t8, 0, 8);
    xsi_vlog_signed_add(t8, 32, t4, 32, t5, 32);
    t6 = (t0 + 2568);
    xsi_vlogvar_wait_assign_value(t6, t8, 0, 0, 32, 0LL);

LAB38:    goto LAB2;

LAB6:    xsi_set_current_line(34, ng0);

LAB9:    xsi_set_current_line(35, ng0);
    t16 = (t0 + 1448);
    t17 = (t16 + 56U);
    t18 = *((char **)t17);
    memset(t15, 0, 8);
    t19 = (t18 + 4);
    t20 = *((unsigned int *)t19);
    t21 = (~(t20));
    t22 = *((unsigned int *)t18);
    t23 = (t22 & t21);
    t24 = (t23 & 1U);
    if (t24 != 0)
        goto LAB13;

LAB11:    if (*((unsigned int *)t19) == 0)
        goto LAB10;

LAB12:    t25 = (t15 + 4);
    *((unsigned int *)t15) = 1;
    *((unsigned int *)t25) = 1;

LAB13:    t26 = (t15 + 4);
    t27 = (t18 + 4);
    t28 = *((unsigned int *)t18);
    t29 = (~(t28));
    *((unsigned int *)t15) = t29;
    *((unsigned int *)t26) = 0;
    if (*((unsigned int *)t27) != 0)
        goto LAB15;

LAB14:    t34 = *((unsigned int *)t15);
    *((unsigned int *)t15) = (t34 & 1U);
    t35 = *((unsigned int *)t26);
    *((unsigned int *)t26) = (t35 & 1U);
    t36 = (t0 + 1448);
    xsi_vlogvar_wait_assign_value(t36, t15, 0, 0, 1, 0LL);
    xsi_set_current_line(36, ng0);
    t2 = ((char*)((ng2)));
    t3 = (t0 + 2088);
    xsi_vlogvar_wait_assign_value(t3, t2, 0, 0, 32, 0LL);
    goto LAB8;

LAB10:    *((unsigned int *)t15) = 1;
    goto LAB13;

LAB15:    t30 = *((unsigned int *)t15);
    t31 = *((unsigned int *)t27);
    *((unsigned int *)t15) = (t30 | t31);
    t32 = *((unsigned int *)t26);
    t33 = *((unsigned int *)t27);
    *((unsigned int *)t26) = (t32 | t33);
    goto LAB14;

LAB16:    xsi_set_current_line(40, ng0);

LAB19:    xsi_set_current_line(41, ng0);
    t7 = (t0 + 1608);
    t9 = (t7 + 56U);
    t16 = *((char **)t9);
    memset(t15, 0, 8);
    t17 = (t16 + 4);
    t20 = *((unsigned int *)t17);
    t21 = (~(t20));
    t22 = *((unsigned int *)t16);
    t23 = (t22 & t21);
    t24 = (t23 & 1U);
    if (t24 != 0)
        goto LAB23;

LAB21:    if (*((unsigned int *)t17) == 0)
        goto LAB20;

LAB22:    t18 = (t15 + 4);
    *((unsigned int *)t15) = 1;
    *((unsigned int *)t18) = 1;

LAB23:    t19 = (t15 + 4);
    t25 = (t16 + 4);
    t28 = *((unsigned int *)t16);
    t29 = (~(t28));
    *((unsigned int *)t15) = t29;
    *((unsigned int *)t19) = 0;
    if (*((unsigned int *)t25) != 0)
        goto LAB25;

LAB24:    t34 = *((unsigned int *)t15);
    *((unsigned int *)t15) = (t34 & 1U);
    t35 = *((unsigned int *)t19);
    *((unsigned int *)t19) = (t35 & 1U);
    t26 = (t0 + 1608);
    xsi_vlogvar_wait_assign_value(t26, t15, 0, 0, 1, 0LL);
    xsi_set_current_line(42, ng0);
    t2 = ((char*)((ng2)));
    t3 = (t0 + 2248);
    xsi_vlogvar_wait_assign_value(t3, t2, 0, 0, 32, 0LL);
    goto LAB18;

LAB20:    *((unsigned int *)t15) = 1;
    goto LAB23;

LAB25:    t30 = *((unsigned int *)t15);
    t31 = *((unsigned int *)t25);
    *((unsigned int *)t15) = (t30 | t31);
    t32 = *((unsigned int *)t19);
    t33 = *((unsigned int *)t25);
    *((unsigned int *)t19) = (t32 | t33);
    goto LAB24;

LAB26:    xsi_set_current_line(46, ng0);

LAB29:    xsi_set_current_line(47, ng0);
    t7 = (t0 + 1768);
    t9 = (t7 + 56U);
    t16 = *((char **)t9);
    memset(t15, 0, 8);
    t17 = (t16 + 4);
    t20 = *((unsigned int *)t17);
    t21 = (~(t20));
    t22 = *((unsigned int *)t16);
    t23 = (t22 & t21);
    t24 = (t23 & 1U);
    if (t24 != 0)
        goto LAB33;

LAB31:    if (*((unsigned int *)t17) == 0)
        goto LAB30;

LAB32:    t18 = (t15 + 4);
    *((unsigned int *)t15) = 1;
    *((unsigned int *)t18) = 1;

LAB33:    t19 = (t15 + 4);
    t25 = (t16 + 4);
    t28 = *((unsigned int *)t16);
    t29 = (~(t28));
    *((unsigned int *)t15) = t29;
    *((unsigned int *)t19) = 0;
    if (*((unsigned int *)t25) != 0)
        goto LAB35;

LAB34:    t34 = *((unsigned int *)t15);
    *((unsigned int *)t15) = (t34 & 1U);
    t35 = *((unsigned int *)t19);
    *((unsigned int *)t19) = (t35 & 1U);
    t26 = (t0 + 1768);
    xsi_vlogvar_wait_assign_value(t26, t15, 0, 0, 1, 0LL);
    xsi_set_current_line(48, ng0);
    t2 = ((char*)((ng2)));
    t3 = (t0 + 2408);
    xsi_vlogvar_wait_assign_value(t3, t2, 0, 0, 32, 0LL);
    goto LAB28;

LAB30:    *((unsigned int *)t15) = 1;
    goto LAB33;

LAB35:    t30 = *((unsigned int *)t15);
    t31 = *((unsigned int *)t25);
    *((unsigned int *)t15) = (t30 | t31);
    t32 = *((unsigned int *)t19);
    t33 = *((unsigned int *)t25);
    *((unsigned int *)t19) = (t32 | t33);
    goto LAB34;

LAB36:    xsi_set_current_line(52, ng0);

LAB39:    xsi_set_current_line(53, ng0);
    t7 = (t0 + 1928);
    t9 = (t7 + 56U);
    t16 = *((char **)t9);
    memset(t15, 0, 8);
    t17 = (t16 + 4);
    t20 = *((unsigned int *)t17);
    t21 = (~(t20));
    t22 = *((unsigned int *)t16);
    t23 = (t22 & t21);
    t24 = (t23 & 1U);
    if (t24 != 0)
        goto LAB43;

LAB41:    if (*((unsigned int *)t17) == 0)
        goto LAB40;

LAB42:    t18 = (t15 + 4);
    *((unsigned int *)t15) = 1;
    *((unsigned int *)t18) = 1;

LAB43:    t19 = (t15 + 4);
    t25 = (t16 + 4);
    t28 = *((unsigned int *)t16);
    t29 = (~(t28));
    *((unsigned int *)t15) = t29;
    *((unsigned int *)t19) = 0;
    if (*((unsigned int *)t25) != 0)
        goto LAB45;

LAB44:    t34 = *((unsigned int *)t15);
    *((unsigned int *)t15) = (t34 & 1U);
    t35 = *((unsigned int *)t19);
    *((unsigned int *)t19) = (t35 & 1U);
    t26 = (t0 + 1928);
    xsi_vlogvar_wait_assign_value(t26, t15, 0, 0, 1, 0LL);
    xsi_set_current_line(54, ng0);
    t2 = ((char*)((ng2)));
    t3 = (t0 + 2568);
    xsi_vlogvar_wait_assign_value(t3, t2, 0, 0, 32, 0LL);
    goto LAB38;

LAB40:    *((unsigned int *)t15) = 1;
    goto LAB43;

LAB45:    t30 = *((unsigned int *)t15);
    t31 = *((unsigned int *)t25);
    *((unsigned int *)t15) = (t30 | t31);
    t32 = *((unsigned int *)t19);
    t33 = *((unsigned int *)t25);
    *((unsigned int *)t19) = (t32 | t33);
    goto LAB44;

}


extern void work_m_00000000002072557680_2964965119_init()
{
	static char *pe[] = {(void *)Always_33_0};
	xsi_register_didat("work_m_00000000002072557680_2964965119", "isim/tb_isim_beh.exe.sim/work/m_00000000002072557680_2964965119.didat");
	xsi_register_executes(pe);
}
