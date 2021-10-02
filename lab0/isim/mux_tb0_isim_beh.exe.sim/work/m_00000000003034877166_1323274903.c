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
static const char *ng0 = "E:/CS152A/lab0/mux.v";
static int ng1[] = {0, 0};
static int ng2[] = {1, 0};
static int ng3[] = {2, 0};
static int ng4[] = {3, 0};
static int ng5[] = {4, 0};
static int ng6[] = {5, 0};
static int ng7[] = {6, 0};
static int ng8[] = {7, 0};



static void Always_36_0(char *t0)
{
    char t4[8];
    char t7[8];
    char t33[8];
    char t51[8];
    char *t1;
    char *t2;
    char *t3;
    char *t5;
    char *t6;
    char *t8;
    unsigned int t9;
    unsigned int t10;
    unsigned int t11;
    unsigned int t12;
    unsigned int t13;
    unsigned int t14;
    char *t15;
    unsigned int t16;
    unsigned int t17;
    unsigned int t18;
    unsigned int t19;
    unsigned int t20;
    char *t21;
    char *t22;
    char *t23;
    unsigned int t24;
    unsigned int t25;
    unsigned int t26;
    unsigned int t27;
    unsigned int t28;
    unsigned int t29;
    unsigned int t30;
    unsigned int t31;
    char *t32;
    unsigned int t34;
    unsigned int t35;
    unsigned int t36;
    unsigned int t37;
    unsigned int t38;
    unsigned int t39;
    unsigned int t40;
    char *t41;
    int t42;
    unsigned int t43;
    unsigned int t44;
    int t45;
    unsigned int t46;
    unsigned int t47;
    unsigned int t48;
    unsigned int t49;
    char *t50;
    unsigned int t52;
    unsigned int t53;
    unsigned int t54;
    unsigned int t55;
    unsigned int t56;
    char *t57;
    char *t58;
    char *t59;
    unsigned int t60;
    unsigned int t61;
    unsigned int t62;
    unsigned int t63;
    unsigned int t64;
    unsigned int t65;
    unsigned int t66;
    unsigned int t67;
    char *t68;
    unsigned int t69;
    unsigned int t70;
    unsigned int t71;
    unsigned int t72;

LAB0:    t1 = (t0 + 3648U);
    t2 = *((char **)t1);
    if (t2 == 0)
        goto LAB2;

LAB3:    goto *t2;

LAB2:    xsi_set_current_line(36, ng0);
    t2 = (t0 + 3968);
    *((int *)t2) = 1;
    t3 = (t0 + 3680);
    *((char **)t3) = t2;
    *((char **)t1) = &&LAB4;

LAB1:    return;
LAB4:    xsi_set_current_line(36, ng0);

LAB5:    xsi_set_current_line(37, ng0);
    t5 = (t0 + 1048U);
    t6 = *((char **)t5);
    memset(t7, 0, 8);
    t5 = (t7 + 4);
    t8 = (t6 + 4);
    t9 = *((unsigned int *)t6);
    t10 = (t9 >> 0);
    t11 = (t10 & 1);
    *((unsigned int *)t7) = t11;
    t12 = *((unsigned int *)t8);
    t13 = (t12 >> 0);
    t14 = (t13 & 1);
    *((unsigned int *)t5) = t14;
    memset(t4, 0, 8);
    t15 = (t7 + 4);
    t16 = *((unsigned int *)t15);
    t17 = (~(t16));
    t18 = *((unsigned int *)t7);
    t19 = (t18 & t17);
    t20 = (t19 & 1U);
    if (t20 != 0)
        goto LAB9;

LAB7:    if (*((unsigned int *)t15) == 0)
        goto LAB6;

LAB8:    t21 = (t4 + 4);
    *((unsigned int *)t4) = 1;
    *((unsigned int *)t21) = 1;

LAB9:    t22 = (t4 + 4);
    t23 = (t7 + 4);
    t24 = *((unsigned int *)t7);
    t25 = (~(t24));
    *((unsigned int *)t4) = t25;
    *((unsigned int *)t22) = 0;
    if (*((unsigned int *)t23) != 0)
        goto LAB11;

LAB10:    t30 = *((unsigned int *)t4);
    *((unsigned int *)t4) = (t30 & 1U);
    t31 = *((unsigned int *)t22);
    *((unsigned int *)t22) = (t31 & 1U);
    t32 = (t0 + 1608);
    xsi_vlogvar_assign_value(t32, t4, 0, 0, 1);
    xsi_set_current_line(38, ng0);
    t2 = (t0 + 1048U);
    t3 = *((char **)t2);
    memset(t4, 0, 8);
    t2 = (t4 + 4);
    t5 = (t3 + 4);
    t9 = *((unsigned int *)t3);
    t10 = (t9 >> 0);
    t11 = (t10 & 1);
    *((unsigned int *)t4) = t11;
    t12 = *((unsigned int *)t5);
    t13 = (t12 >> 0);
    t14 = (t13 & 1);
    *((unsigned int *)t2) = t14;
    t6 = (t0 + 1768);
    xsi_vlogvar_assign_value(t6, t4, 0, 0, 1);
    xsi_set_current_line(39, ng0);
    t2 = (t0 + 1048U);
    t3 = *((char **)t2);
    memset(t4, 0, 8);
    t2 = (t4 + 4);
    t5 = (t3 + 4);
    t9 = *((unsigned int *)t3);
    t10 = (t9 >> 1);
    t11 = (t10 & 1);
    *((unsigned int *)t4) = t11;
    t12 = *((unsigned int *)t5);
    t13 = (t12 >> 1);
    t14 = (t13 & 1);
    *((unsigned int *)t2) = t14;
    t6 = (t0 + 1048U);
    t8 = *((char **)t6);
    memset(t7, 0, 8);
    t6 = (t7 + 4);
    t15 = (t8 + 4);
    t16 = *((unsigned int *)t8);
    t17 = (t16 >> 0);
    t18 = (t17 & 1);
    *((unsigned int *)t7) = t18;
    t19 = *((unsigned int *)t15);
    t20 = (t19 >> 0);
    t24 = (t20 & 1);
    *((unsigned int *)t6) = t24;
    t25 = *((unsigned int *)t4);
    t26 = *((unsigned int *)t7);
    t27 = (t25 ^ t26);
    *((unsigned int *)t33) = t27;
    t21 = (t4 + 4);
    t22 = (t7 + 4);
    t23 = (t33 + 4);
    t28 = *((unsigned int *)t21);
    t29 = *((unsigned int *)t22);
    t30 = (t28 | t29);
    *((unsigned int *)t23) = t30;
    t31 = *((unsigned int *)t33);
    t34 = (~(t31));
    *((unsigned int *)t33) = t34;
    t35 = *((unsigned int *)t23);
    t36 = (t35 != 0);
    if (t36 == 1)
        goto LAB12;

LAB13:
LAB14:    t39 = *((unsigned int *)t33);
    *((unsigned int *)t33) = (t39 & 1U);
    t32 = (t33 + 4);
    t40 = *((unsigned int *)t32);
    *((unsigned int *)t32) = (t40 & 1U);
    t41 = (t0 + 1928);
    xsi_vlogvar_assign_value(t41, t33, 0, 0, 1);
    xsi_set_current_line(40, ng0);
    t2 = (t0 + 1048U);
    t3 = *((char **)t2);
    memset(t4, 0, 8);
    t2 = (t4 + 4);
    t5 = (t3 + 4);
    t9 = *((unsigned int *)t3);
    t10 = (t9 >> 1);
    t11 = (t10 & 1);
    *((unsigned int *)t4) = t11;
    t12 = *((unsigned int *)t5);
    t13 = (t12 >> 1);
    t14 = (t13 & 1);
    *((unsigned int *)t2) = t14;
    t6 = (t0 + 1048U);
    t8 = *((char **)t6);
    memset(t7, 0, 8);
    t6 = (t7 + 4);
    t15 = (t8 + 4);
    t16 = *((unsigned int *)t8);
    t17 = (t16 >> 0);
    t18 = (t17 & 1);
    *((unsigned int *)t7) = t18;
    t19 = *((unsigned int *)t15);
    t20 = (t19 >> 0);
    t24 = (t20 & 1);
    *((unsigned int *)t6) = t24;
    t25 = *((unsigned int *)t4);
    t26 = *((unsigned int *)t7);
    t27 = (t25 ^ t26);
    *((unsigned int *)t33) = t27;
    t21 = (t4 + 4);
    t22 = (t7 + 4);
    t23 = (t33 + 4);
    t28 = *((unsigned int *)t21);
    t29 = *((unsigned int *)t22);
    t30 = (t28 | t29);
    *((unsigned int *)t23) = t30;
    t31 = *((unsigned int *)t23);
    t34 = (t31 != 0);
    if (t34 == 1)
        goto LAB15;

LAB16:
LAB17:    t32 = (t0 + 2088);
    xsi_vlogvar_assign_value(t32, t33, 0, 0, 1);
    xsi_set_current_line(41, ng0);
    t2 = (t0 + 1048U);
    t3 = *((char **)t2);
    memset(t4, 0, 8);
    t2 = (t4 + 4);
    t5 = (t3 + 4);
    t9 = *((unsigned int *)t3);
    t10 = (t9 >> 1);
    t11 = (t10 & 1);
    *((unsigned int *)t4) = t11;
    t12 = *((unsigned int *)t5);
    t13 = (t12 >> 1);
    t14 = (t13 & 1);
    *((unsigned int *)t2) = t14;
    t6 = (t0 + 1048U);
    t8 = *((char **)t6);
    memset(t7, 0, 8);
    t6 = (t7 + 4);
    t15 = (t8 + 4);
    t16 = *((unsigned int *)t8);
    t17 = (t16 >> 0);
    t18 = (t17 & 1);
    *((unsigned int *)t7) = t18;
    t19 = *((unsigned int *)t15);
    t20 = (t19 >> 0);
    t24 = (t20 & 1);
    *((unsigned int *)t6) = t24;
    t25 = *((unsigned int *)t4);
    t26 = *((unsigned int *)t7);
    t27 = (t25 | t26);
    *((unsigned int *)t33) = t27;
    t21 = (t4 + 4);
    t22 = (t7 + 4);
    t23 = (t33 + 4);
    t28 = *((unsigned int *)t21);
    t29 = *((unsigned int *)t22);
    t30 = (t28 | t29);
    *((unsigned int *)t23) = t30;
    t31 = *((unsigned int *)t23);
    t34 = (t31 != 0);
    if (t34 == 1)
        goto LAB18;

LAB19:
LAB20:    t50 = (t0 + 2248);
    xsi_vlogvar_assign_value(t50, t33, 0, 0, 1);
    xsi_set_current_line(42, ng0);
    t2 = (t0 + 1048U);
    t3 = *((char **)t2);
    memset(t7, 0, 8);
    t2 = (t7 + 4);
    t5 = (t3 + 4);
    t9 = *((unsigned int *)t3);
    t10 = (t9 >> 1);
    t11 = (t10 & 1);
    *((unsigned int *)t7) = t11;
    t12 = *((unsigned int *)t5);
    t13 = (t12 >> 1);
    t14 = (t13 & 1);
    *((unsigned int *)t2) = t14;
    t6 = (t0 + 1048U);
    t8 = *((char **)t6);
    memset(t33, 0, 8);
    t6 = (t33 + 4);
    t15 = (t8 + 4);
    t16 = *((unsigned int *)t8);
    t17 = (t16 >> 0);
    t18 = (t17 & 1);
    *((unsigned int *)t33) = t18;
    t19 = *((unsigned int *)t15);
    t20 = (t19 >> 0);
    t24 = (t20 & 1);
    *((unsigned int *)t6) = t24;
    t25 = *((unsigned int *)t7);
    t26 = *((unsigned int *)t33);
    t27 = (t25 | t26);
    *((unsigned int *)t51) = t27;
    t21 = (t7 + 4);
    t22 = (t33 + 4);
    t23 = (t51 + 4);
    t28 = *((unsigned int *)t21);
    t29 = *((unsigned int *)t22);
    t30 = (t28 | t29);
    *((unsigned int *)t23) = t30;
    t31 = *((unsigned int *)t23);
    t34 = (t31 != 0);
    if (t34 == 1)
        goto LAB21;

LAB22:
LAB23:    memset(t4, 0, 8);
    t50 = (t51 + 4);
    t52 = *((unsigned int *)t50);
    t53 = (~(t52));
    t54 = *((unsigned int *)t51);
    t55 = (t54 & t53);
    t56 = (t55 & 1U);
    if (t56 != 0)
        goto LAB27;

LAB25:    if (*((unsigned int *)t50) == 0)
        goto LAB24;

LAB26:    t57 = (t4 + 4);
    *((unsigned int *)t4) = 1;
    *((unsigned int *)t57) = 1;

LAB27:    t58 = (t4 + 4);
    t59 = (t51 + 4);
    t60 = *((unsigned int *)t51);
    t61 = (~(t60));
    *((unsigned int *)t4) = t61;
    *((unsigned int *)t58) = 0;
    if (*((unsigned int *)t59) != 0)
        goto LAB29;

LAB28:    t66 = *((unsigned int *)t4);
    *((unsigned int *)t4) = (t66 & 1U);
    t67 = *((unsigned int *)t58);
    *((unsigned int *)t58) = (t67 & 1U);
    t68 = (t0 + 2408);
    xsi_vlogvar_assign_value(t68, t4, 0, 0, 1);
    xsi_set_current_line(43, ng0);
    t2 = (t0 + 1048U);
    t3 = *((char **)t2);
    memset(t4, 0, 8);
    t2 = (t4 + 4);
    t5 = (t3 + 4);
    t9 = *((unsigned int *)t3);
    t10 = (t9 >> 1);
    t11 = (t10 & 1);
    *((unsigned int *)t4) = t11;
    t12 = *((unsigned int *)t5);
    t13 = (t12 >> 1);
    t14 = (t13 & 1);
    *((unsigned int *)t2) = t14;
    t6 = (t0 + 1048U);
    t8 = *((char **)t6);
    memset(t7, 0, 8);
    t6 = (t7 + 4);
    t15 = (t8 + 4);
    t16 = *((unsigned int *)t8);
    t17 = (t16 >> 0);
    t18 = (t17 & 1);
    *((unsigned int *)t7) = t18;
    t19 = *((unsigned int *)t15);
    t20 = (t19 >> 0);
    t24 = (t20 & 1);
    *((unsigned int *)t6) = t24;
    t25 = *((unsigned int *)t4);
    t26 = *((unsigned int *)t7);
    t27 = (t25 & t26);
    *((unsigned int *)t33) = t27;
    t21 = (t4 + 4);
    t22 = (t7 + 4);
    t23 = (t33 + 4);
    t28 = *((unsigned int *)t21);
    t29 = *((unsigned int *)t22);
    t30 = (t28 | t29);
    *((unsigned int *)t23) = t30;
    t31 = *((unsigned int *)t23);
    t34 = (t31 != 0);
    if (t34 == 1)
        goto LAB30;

LAB31:
LAB32:    t50 = (t0 + 2568);
    xsi_vlogvar_assign_value(t50, t33, 0, 0, 1);
    xsi_set_current_line(44, ng0);
    t2 = (t0 + 1048U);
    t3 = *((char **)t2);
    memset(t7, 0, 8);
    t2 = (t7 + 4);
    t5 = (t3 + 4);
    t9 = *((unsigned int *)t3);
    t10 = (t9 >> 1);
    t11 = (t10 & 1);
    *((unsigned int *)t7) = t11;
    t12 = *((unsigned int *)t5);
    t13 = (t12 >> 1);
    t14 = (t13 & 1);
    *((unsigned int *)t2) = t14;
    t6 = (t0 + 1048U);
    t8 = *((char **)t6);
    memset(t33, 0, 8);
    t6 = (t33 + 4);
    t15 = (t8 + 4);
    t16 = *((unsigned int *)t8);
    t17 = (t16 >> 0);
    t18 = (t17 & 1);
    *((unsigned int *)t33) = t18;
    t19 = *((unsigned int *)t15);
    t20 = (t19 >> 0);
    t24 = (t20 & 1);
    *((unsigned int *)t6) = t24;
    t25 = *((unsigned int *)t7);
    t26 = *((unsigned int *)t33);
    t27 = (t25 & t26);
    *((unsigned int *)t51) = t27;
    t21 = (t7 + 4);
    t22 = (t33 + 4);
    t23 = (t51 + 4);
    t28 = *((unsigned int *)t21);
    t29 = *((unsigned int *)t22);
    t30 = (t28 | t29);
    *((unsigned int *)t23) = t30;
    t31 = *((unsigned int *)t23);
    t34 = (t31 != 0);
    if (t34 == 1)
        goto LAB33;

LAB34:
LAB35:    memset(t4, 0, 8);
    t50 = (t51 + 4);
    t56 = *((unsigned int *)t50);
    t60 = (~(t56));
    t61 = *((unsigned int *)t51);
    t62 = (t61 & t60);
    t63 = (t62 & 1U);
    if (t63 != 0)
        goto LAB39;

LAB37:    if (*((unsigned int *)t50) == 0)
        goto LAB36;

LAB38:    t57 = (t4 + 4);
    *((unsigned int *)t4) = 1;
    *((unsigned int *)t57) = 1;

LAB39:    t58 = (t4 + 4);
    t59 = (t51 + 4);
    t64 = *((unsigned int *)t51);
    t65 = (~(t64));
    *((unsigned int *)t4) = t65;
    *((unsigned int *)t58) = 0;
    if (*((unsigned int *)t59) != 0)
        goto LAB41;

LAB40:    t71 = *((unsigned int *)t4);
    *((unsigned int *)t4) = (t71 & 1U);
    t72 = *((unsigned int *)t58);
    *((unsigned int *)t58) = (t72 & 1U);
    t68 = (t0 + 2728);
    xsi_vlogvar_assign_value(t68, t4, 0, 0, 1);
    xsi_set_current_line(46, ng0);
    t2 = (t0 + 1048U);
    t3 = *((char **)t2);
    memset(t4, 0, 8);
    t2 = (t4 + 4);
    t5 = (t3 + 4);
    t9 = *((unsigned int *)t3);
    t10 = (t9 >> 2);
    *((unsigned int *)t4) = t10;
    t11 = *((unsigned int *)t5);
    t12 = (t11 >> 2);
    *((unsigned int *)t2) = t12;
    t13 = *((unsigned int *)t4);
    *((unsigned int *)t4) = (t13 & 7U);
    t14 = *((unsigned int *)t2);
    *((unsigned int *)t2) = (t14 & 7U);

LAB42:    t6 = ((char*)((ng1)));
    t42 = xsi_vlog_unsigned_case_compare(t4, 32, t6, 32);
    if (t42 == 1)
        goto LAB43;

LAB44:    t2 = ((char*)((ng2)));
    t42 = xsi_vlog_unsigned_case_compare(t4, 32, t2, 32);
    if (t42 == 1)
        goto LAB45;

LAB46:    t2 = ((char*)((ng3)));
    t42 = xsi_vlog_unsigned_case_compare(t4, 32, t2, 32);
    if (t42 == 1)
        goto LAB47;

LAB48:    t2 = ((char*)((ng4)));
    t42 = xsi_vlog_unsigned_case_compare(t4, 32, t2, 32);
    if (t42 == 1)
        goto LAB49;

LAB50:    t2 = ((char*)((ng5)));
    t42 = xsi_vlog_unsigned_case_compare(t4, 32, t2, 32);
    if (t42 == 1)
        goto LAB51;

LAB52:    t2 = ((char*)((ng6)));
    t42 = xsi_vlog_unsigned_case_compare(t4, 32, t2, 32);
    if (t42 == 1)
        goto LAB53;

LAB54:    t2 = ((char*)((ng7)));
    t42 = xsi_vlog_unsigned_case_compare(t4, 32, t2, 32);
    if (t42 == 1)
        goto LAB55;

LAB56:    t2 = ((char*)((ng8)));
    t42 = xsi_vlog_unsigned_case_compare(t4, 32, t2, 32);
    if (t42 == 1)
        goto LAB57;

LAB58:
LAB59:    goto LAB2;

LAB6:    *((unsigned int *)t4) = 1;
    goto LAB9;

LAB11:    t26 = *((unsigned int *)t4);
    t27 = *((unsigned int *)t23);
    *((unsigned int *)t4) = (t26 | t27);
    t28 = *((unsigned int *)t22);
    t29 = *((unsigned int *)t23);
    *((unsigned int *)t22) = (t28 | t29);
    goto LAB10;

LAB12:    t37 = *((unsigned int *)t33);
    t38 = *((unsigned int *)t23);
    *((unsigned int *)t33) = (t37 | t38);
    goto LAB14;

LAB15:    t35 = *((unsigned int *)t33);
    t36 = *((unsigned int *)t23);
    *((unsigned int *)t33) = (t35 | t36);
    goto LAB17;

LAB18:    t35 = *((unsigned int *)t33);
    t36 = *((unsigned int *)t23);
    *((unsigned int *)t33) = (t35 | t36);
    t32 = (t4 + 4);
    t41 = (t7 + 4);
    t37 = *((unsigned int *)t32);
    t38 = (~(t37));
    t39 = *((unsigned int *)t4);
    t42 = (t39 & t38);
    t40 = *((unsigned int *)t41);
    t43 = (~(t40));
    t44 = *((unsigned int *)t7);
    t45 = (t44 & t43);
    t46 = (~(t42));
    t47 = (~(t45));
    t48 = *((unsigned int *)t23);
    *((unsigned int *)t23) = (t48 & t46);
    t49 = *((unsigned int *)t23);
    *((unsigned int *)t23) = (t49 & t47);
    goto LAB20;

LAB21:    t35 = *((unsigned int *)t51);
    t36 = *((unsigned int *)t23);
    *((unsigned int *)t51) = (t35 | t36);
    t32 = (t7 + 4);
    t41 = (t33 + 4);
    t37 = *((unsigned int *)t32);
    t38 = (~(t37));
    t39 = *((unsigned int *)t7);
    t42 = (t39 & t38);
    t40 = *((unsigned int *)t41);
    t43 = (~(t40));
    t44 = *((unsigned int *)t33);
    t45 = (t44 & t43);
    t46 = (~(t42));
    t47 = (~(t45));
    t48 = *((unsigned int *)t23);
    *((unsigned int *)t23) = (t48 & t46);
    t49 = *((unsigned int *)t23);
    *((unsigned int *)t23) = (t49 & t47);
    goto LAB23;

LAB24:    *((unsigned int *)t4) = 1;
    goto LAB27;

LAB29:    t62 = *((unsigned int *)t4);
    t63 = *((unsigned int *)t59);
    *((unsigned int *)t4) = (t62 | t63);
    t64 = *((unsigned int *)t58);
    t65 = *((unsigned int *)t59);
    *((unsigned int *)t58) = (t64 | t65);
    goto LAB28;

LAB30:    t35 = *((unsigned int *)t33);
    t36 = *((unsigned int *)t23);
    *((unsigned int *)t33) = (t35 | t36);
    t32 = (t4 + 4);
    t41 = (t7 + 4);
    t37 = *((unsigned int *)t4);
    t38 = (~(t37));
    t39 = *((unsigned int *)t32);
    t40 = (~(t39));
    t43 = *((unsigned int *)t7);
    t44 = (~(t43));
    t46 = *((unsigned int *)t41);
    t47 = (~(t46));
    t42 = (t38 & t40);
    t45 = (t44 & t47);
    t48 = (~(t42));
    t49 = (~(t45));
    t52 = *((unsigned int *)t23);
    *((unsigned int *)t23) = (t52 & t48);
    t53 = *((unsigned int *)t23);
    *((unsigned int *)t23) = (t53 & t49);
    t54 = *((unsigned int *)t33);
    *((unsigned int *)t33) = (t54 & t48);
    t55 = *((unsigned int *)t33);
    *((unsigned int *)t33) = (t55 & t49);
    goto LAB32;

LAB33:    t35 = *((unsigned int *)t51);
    t36 = *((unsigned int *)t23);
    *((unsigned int *)t51) = (t35 | t36);
    t32 = (t7 + 4);
    t41 = (t33 + 4);
    t37 = *((unsigned int *)t7);
    t38 = (~(t37));
    t39 = *((unsigned int *)t32);
    t40 = (~(t39));
    t43 = *((unsigned int *)t33);
    t44 = (~(t43));
    t46 = *((unsigned int *)t41);
    t47 = (~(t46));
    t42 = (t38 & t40);
    t45 = (t44 & t47);
    t48 = (~(t42));
    t49 = (~(t45));
    t52 = *((unsigned int *)t23);
    *((unsigned int *)t23) = (t52 & t48);
    t53 = *((unsigned int *)t23);
    *((unsigned int *)t23) = (t53 & t49);
    t54 = *((unsigned int *)t51);
    *((unsigned int *)t51) = (t54 & t48);
    t55 = *((unsigned int *)t51);
    *((unsigned int *)t51) = (t55 & t49);
    goto LAB35;

LAB36:    *((unsigned int *)t4) = 1;
    goto LAB39;

LAB41:    t66 = *((unsigned int *)t4);
    t67 = *((unsigned int *)t59);
    *((unsigned int *)t4) = (t66 | t67);
    t69 = *((unsigned int *)t58);
    t70 = *((unsigned int *)t59);
    *((unsigned int *)t58) = (t69 | t70);
    goto LAB40;

LAB43:    xsi_set_current_line(47, ng0);
    t8 = (t0 + 1608);
    t15 = (t8 + 56U);
    t21 = *((char **)t15);
    t22 = (t0 + 1448);
    xsi_vlogvar_assign_value(t22, t21, 0, 0, 1);
    goto LAB59;

LAB45:    xsi_set_current_line(48, ng0);
    t3 = (t0 + 1768);
    t5 = (t3 + 56U);
    t6 = *((char **)t5);
    t8 = (t0 + 1448);
    xsi_vlogvar_assign_value(t8, t6, 0, 0, 1);
    goto LAB59;

LAB47:    xsi_set_current_line(49, ng0);
    t3 = (t0 + 1928);
    t5 = (t3 + 56U);
    t6 = *((char **)t5);
    t8 = (t0 + 1448);
    xsi_vlogvar_assign_value(t8, t6, 0, 0, 1);
    goto LAB59;

LAB49:    xsi_set_current_line(50, ng0);
    t3 = (t0 + 2088);
    t5 = (t3 + 56U);
    t6 = *((char **)t5);
    t8 = (t0 + 1448);
    xsi_vlogvar_assign_value(t8, t6, 0, 0, 1);
    goto LAB59;

LAB51:    xsi_set_current_line(51, ng0);
    t3 = (t0 + 2248);
    t5 = (t3 + 56U);
    t6 = *((char **)t5);
    t8 = (t0 + 1448);
    xsi_vlogvar_assign_value(t8, t6, 0, 0, 1);
    goto LAB59;

LAB53:    xsi_set_current_line(52, ng0);
    t3 = (t0 + 2408);
    t5 = (t3 + 56U);
    t6 = *((char **)t5);
    t8 = (t0 + 1448);
    xsi_vlogvar_assign_value(t8, t6, 0, 0, 1);
    goto LAB59;

LAB55:    xsi_set_current_line(53, ng0);
    t3 = (t0 + 2568);
    t5 = (t3 + 56U);
    t6 = *((char **)t5);
    t8 = (t0 + 1448);
    xsi_vlogvar_assign_value(t8, t6, 0, 0, 1);
    goto LAB59;

LAB57:    xsi_set_current_line(54, ng0);
    t3 = (t0 + 2728);
    t5 = (t3 + 56U);
    t6 = *((char **)t5);
    t8 = (t0 + 1448);
    xsi_vlogvar_assign_value(t8, t6, 0, 0, 1);
    goto LAB59;

}


extern void work_m_00000000003034877166_1323274903_init()
{
	static char *pe[] = {(void *)Always_36_0};
	xsi_register_didat("work_m_00000000003034877166_1323274903", "isim/mux_tb0_isim_beh.exe.sim/work/m_00000000003034877166_1323274903.didat");
	xsi_register_executes(pe);
}
