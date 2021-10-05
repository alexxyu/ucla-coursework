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
static const char *ng0 = "E:/CS152A/lab1/tb/tb.v";
static const char *ng1 = "%d ... Running instruction %08b";
static int ng2[] = {1, 0};
static int ng3[] = {0, 0};
static unsigned int ng4[] = {0U, 0U};
static unsigned int ng5[] = {3U, 0U};
static unsigned int ng6[] = {1U, 0U};
static unsigned int ng7[] = {2U, 0U};
static const char *ng8 = "seq.code";
static const char *ng9 = "r";
static const char *ng10 = "%b\n";
static const char *ng11 = "%d ... instruction %08b executed";
static const char *ng12 = "%d ... led output changed to %08b";



static int sp_tskRunInst(char *t1, char *t2)
{
    char t5[8];
    int t0;
    char *t3;
    char *t4;
    char *t6;
    char *t7;
    char *t8;
    char *t9;
    char *t10;

LAB0:    t0 = 1;
    t3 = (t2 + 48U);
    t4 = *((char **)t3);
    if (t4 == 0)
        goto LAB2;

LAB3:    goto *t4;

LAB2:    t4 = (t1 + 848);
    xsi_vlog_subprogram_setdisablestate(t4, &&LAB4);
    xsi_set_current_line(91, ng0);

LAB5:    xsi_set_current_line(92, ng0);
    *((int *)t5) = xsi_vlog_stime(1000.0000000000000, 1000.0000000000000);
    t6 = (t5 + 4);
    *((int *)t6) = 0;
    t7 = (t1 + 5368);
    t8 = (t7 + 56U);
    t9 = *((char **)t8);
    t10 = (t1 + 848);
    xsi_vlogfile_write(1, 0, 0, ng1, 3, t10, (char)118, t5, 32, (char)118, t9, 8);
    xsi_set_current_line(93, ng0);
    t4 = (t1 + 5368);
    t6 = (t4 + 56U);
    t7 = *((char **)t6);
    t8 = (t1 + 3928);
    xsi_vlogvar_assign_value(t8, t7, 0, 0, 8);
    xsi_set_current_line(94, ng0);
    t4 = (t2 + 56U);
    t6 = *((char **)t4);
    xsi_process_wait(t6, 1500000000LL);
    *((char **)t3) = &&LAB6;
    t0 = 1;

LAB1:    return t0;
LAB4:    xsi_vlog_dispose_subprogram_invocation(t2);
    t4 = (t2 + 48U);
    *((char **)t4) = &&LAB2;
    t0 = 0;
    goto LAB1;

LAB6:    xsi_set_current_line(94, ng0);
    t7 = ((char*)((ng2)));
    t8 = (t1 + 4248);
    xsi_vlogvar_assign_value(t8, t7, 0, 0, 1);
    xsi_set_current_line(95, ng0);
    t4 = (t2 + 56U);
    t6 = *((char **)t4);
    xsi_process_wait(t6, 3000000000LL);
    *((char **)t3) = &&LAB7;
    t0 = 1;
    goto LAB1;

LAB7:    xsi_set_current_line(95, ng0);
    t7 = ((char*)((ng3)));
    t8 = (t1 + 4248);
    xsi_vlogvar_assign_value(t8, t7, 0, 0, 1);
    goto LAB4;

}

static int sp_tskRunPUSH(char *t1, char *t2)
{
    char t5[8];
    char t6[8];
    char t18[8];
    int t0;
    char *t3;
    char *t4;
    char *t7;
    char *t8;
    char *t9;
    char *t10;
    char *t11;
    unsigned int t12;
    unsigned int t13;
    unsigned int t14;
    unsigned int t15;
    unsigned int t16;
    unsigned int t17;
    char *t19;
    char *t20;
    char *t21;
    char *t22;
    char *t23;
    unsigned int t24;
    unsigned int t25;
    unsigned int t26;
    unsigned int t27;
    unsigned int t28;
    unsigned int t29;
    char *t30;
    char *t31;
    char *t32;
    char *t33;
    char *t34;
    char *t35;
    char *t36;
    char *t37;
    char *t38;
    int t39;
    char *t40;
    char *t41;
    char *t42;
    char *t43;

LAB0:    t0 = 1;
    t3 = (t2 + 48U);
    t4 = *((char **)t3);
    if (t4 == 0)
        goto LAB2;

LAB3:    goto *t4;

LAB2:    t4 = (t1 + 1280);
    xsi_vlog_subprogram_setdisablestate(t4, &&LAB4);
    xsi_set_current_line(103, ng0);

LAB5:    xsi_set_current_line(104, ng0);
    t7 = (t1 + 5688);
    t8 = (t7 + 56U);
    t9 = *((char **)t8);
    memset(t6, 0, 8);
    t10 = (t6 + 4);
    t11 = (t9 + 4);
    t12 = *((unsigned int *)t9);
    t13 = (t12 >> 0);
    *((unsigned int *)t6) = t13;
    t14 = *((unsigned int *)t11);
    t15 = (t14 >> 0);
    *((unsigned int *)t10) = t15;
    t16 = *((unsigned int *)t6);
    *((unsigned int *)t6) = (t16 & 15U);
    t17 = *((unsigned int *)t10);
    *((unsigned int *)t10) = (t17 & 15U);
    t19 = (t1 + 5528);
    t20 = (t19 + 56U);
    t21 = *((char **)t20);
    memset(t18, 0, 8);
    t22 = (t18 + 4);
    t23 = (t21 + 4);
    t24 = *((unsigned int *)t21);
    t25 = (t24 >> 0);
    *((unsigned int *)t18) = t25;
    t26 = *((unsigned int *)t23);
    t27 = (t26 >> 0);
    *((unsigned int *)t22) = t27;
    t28 = *((unsigned int *)t18);
    *((unsigned int *)t18) = (t28 & 3U);
    t29 = *((unsigned int *)t22);
    *((unsigned int *)t22) = (t29 & 3U);
    t30 = ((char*)((ng4)));
    xsi_vlogtype_concat(t5, 8, 8, 3U, t30, 2, t18, 2, t6, 4);
    t31 = (t1 + 5848);
    xsi_vlogvar_assign_value(t31, t5, 0, 0, 8);
    xsi_set_current_line(105, ng0);
    t4 = (t1 + 5848);
    t7 = (t4 + 56U);
    t8 = *((char **)t7);
    t9 = (t2 + 56U);
    t10 = *((char **)t9);
    t11 = (t2 + 56U);
    t19 = *((char **)t11);
    xsi_vlog_subprograminvocation_setJumpstate(t2, t19, &&LAB6);
    t20 = (t2 + 56U);
    t21 = *((char **)t20);
    t22 = (t1 + 848);
    t23 = xsi_create_subprogram_invocation(t21, 0, t1, t22, 0, t2);
    xsi_vlog_subprogram_pushinvocation(t22, t23);
    t30 = (t1 + 5368);
    xsi_vlogvar_assign_value(t30, t8, 0, 0, 8);

LAB8:    t31 = (t2 + 64U);
    t32 = *((char **)t31);
    t33 = (t32 + 80U);
    t34 = *((char **)t33);
    t35 = (t34 + 272U);
    t36 = *((char **)t35);
    t37 = (t36 + 0U);
    t38 = *((char **)t37);
    t39 = ((int  (*)(char *, char *))t38)(t1, t32);
    if (t39 == -1)
        goto LAB9;

LAB10:    if (t39 != 0)
        goto LAB11;

LAB6:    t32 = (t1 + 848);
    xsi_vlog_subprogram_popinvocation(t32);

LAB7:    t40 = (t2 + 64U);
    t41 = *((char **)t40);
    t40 = (t1 + 848);
    t42 = (t2 + 56U);
    t43 = *((char **)t42);
    xsi_delete_subprogram_invocation(t40, t41, t1, t43, t2);

LAB4:    xsi_vlog_dispose_subprogram_invocation(t2);
    t4 = (t2 + 48U);
    *((char **)t4) = &&LAB2;
    t0 = 0;

LAB1:    return t0;
LAB9:    t0 = -1;
    goto LAB1;

LAB11:    t31 = (t2 + 48U);
    *((char **)t31) = &&LAB8;
    goto LAB1;

}

static int sp_tskRunSEND(char *t1, char *t2)
{
    char t5[8];
    char t7[8];
    int t0;
    char *t3;
    char *t4;
    char *t6;
    char *t8;
    char *t9;
    char *t10;
    char *t11;
    char *t12;
    unsigned int t13;
    unsigned int t14;
    unsigned int t15;
    unsigned int t16;
    unsigned int t17;
    unsigned int t18;
    char *t19;
    char *t20;
    char *t21;
    char *t22;
    char *t23;
    char *t24;
    char *t25;
    char *t26;
    char *t27;
    char *t28;
    char *t29;
    char *t30;
    char *t31;
    int t32;
    char *t33;
    char *t34;
    char *t35;
    char *t36;

LAB0:    t0 = 1;
    t3 = (t2 + 48U);
    t4 = *((char **)t3);
    if (t4 == 0)
        goto LAB2;

LAB3:    goto *t4;

LAB2:    t4 = (t1 + 1712);
    xsi_vlog_subprogram_setdisablestate(t4, &&LAB4);
    xsi_set_current_line(112, ng0);

LAB5:    xsi_set_current_line(113, ng0);
    t6 = ((char*)((ng4)));
    t8 = (t1 + 6008);
    t9 = (t8 + 56U);
    t10 = *((char **)t9);
    memset(t7, 0, 8);
    t11 = (t7 + 4);
    t12 = (t10 + 4);
    t13 = *((unsigned int *)t10);
    t14 = (t13 >> 0);
    *((unsigned int *)t7) = t14;
    t15 = *((unsigned int *)t12);
    t16 = (t15 >> 0);
    *((unsigned int *)t11) = t16;
    t17 = *((unsigned int *)t7);
    *((unsigned int *)t7) = (t17 & 3U);
    t18 = *((unsigned int *)t11);
    *((unsigned int *)t11) = (t18 & 3U);
    t19 = ((char*)((ng5)));
    xsi_vlogtype_concat(t5, 8, 8, 3U, t19, 2, t7, 2, t6, 4);
    t20 = (t1 + 6168);
    xsi_vlogvar_assign_value(t20, t5, 0, 0, 8);
    xsi_set_current_line(114, ng0);
    t4 = (t1 + 6168);
    t6 = (t4 + 56U);
    t8 = *((char **)t6);
    t9 = (t2 + 56U);
    t10 = *((char **)t9);
    t11 = (t2 + 56U);
    t12 = *((char **)t11);
    xsi_vlog_subprograminvocation_setJumpstate(t2, t12, &&LAB6);
    t19 = (t2 + 56U);
    t20 = *((char **)t19);
    t21 = (t1 + 848);
    t22 = xsi_create_subprogram_invocation(t20, 0, t1, t21, 0, t2);
    xsi_vlog_subprogram_pushinvocation(t21, t22);
    t23 = (t1 + 5368);
    xsi_vlogvar_assign_value(t23, t8, 0, 0, 8);

LAB8:    t24 = (t2 + 64U);
    t25 = *((char **)t24);
    t26 = (t25 + 80U);
    t27 = *((char **)t26);
    t28 = (t27 + 272U);
    t29 = *((char **)t28);
    t30 = (t29 + 0U);
    t31 = *((char **)t30);
    t32 = ((int  (*)(char *, char *))t31)(t1, t25);
    if (t32 == -1)
        goto LAB9;

LAB10:    if (t32 != 0)
        goto LAB11;

LAB6:    t25 = (t1 + 848);
    xsi_vlog_subprogram_popinvocation(t25);

LAB7:    t33 = (t2 + 64U);
    t34 = *((char **)t33);
    t33 = (t1 + 848);
    t35 = (t2 + 56U);
    t36 = *((char **)t35);
    xsi_delete_subprogram_invocation(t33, t34, t1, t36, t2);

LAB4:    xsi_vlog_dispose_subprogram_invocation(t2);
    t4 = (t2 + 48U);
    *((char **)t4) = &&LAB2;
    t0 = 0;

LAB1:    return t0;
LAB9:    t0 = -1;
    goto LAB1;

LAB11:    t24 = (t2 + 48U);
    *((char **)t24) = &&LAB8;
    goto LAB1;

}

static int sp_tskRunADD(char *t1, char *t2)
{
    char t5[8];
    char t6[8];
    char t18[8];
    char t30[8];
    int t0;
    char *t3;
    char *t4;
    char *t7;
    char *t8;
    char *t9;
    char *t10;
    char *t11;
    unsigned int t12;
    unsigned int t13;
    unsigned int t14;
    unsigned int t15;
    unsigned int t16;
    unsigned int t17;
    char *t19;
    char *t20;
    char *t21;
    char *t22;
    char *t23;
    unsigned int t24;
    unsigned int t25;
    unsigned int t26;
    unsigned int t27;
    unsigned int t28;
    unsigned int t29;
    char *t31;
    char *t32;
    char *t33;
    char *t34;
    char *t35;
    unsigned int t36;
    unsigned int t37;
    unsigned int t38;
    unsigned int t39;
    unsigned int t40;
    unsigned int t41;
    char *t42;
    char *t43;
    char *t44;
    char *t45;
    int t46;
    char *t47;
    char *t48;
    char *t49;
    char *t50;

LAB0:    t0 = 1;
    t3 = (t2 + 48U);
    t4 = *((char **)t3);
    if (t4 == 0)
        goto LAB2;

LAB3:    goto *t4;

LAB2:    t4 = (t1 + 2144);
    xsi_vlog_subprogram_setdisablestate(t4, &&LAB4);
    xsi_set_current_line(123, ng0);

LAB5:    xsi_set_current_line(124, ng0);
    t7 = (t1 + 6648);
    t8 = (t7 + 56U);
    t9 = *((char **)t8);
    memset(t6, 0, 8);
    t10 = (t6 + 4);
    t11 = (t9 + 4);
    t12 = *((unsigned int *)t9);
    t13 = (t12 >> 0);
    *((unsigned int *)t6) = t13;
    t14 = *((unsigned int *)t11);
    t15 = (t14 >> 0);
    *((unsigned int *)t10) = t15;
    t16 = *((unsigned int *)t6);
    *((unsigned int *)t6) = (t16 & 3U);
    t17 = *((unsigned int *)t10);
    *((unsigned int *)t10) = (t17 & 3U);
    t19 = (t1 + 6488);
    t20 = (t19 + 56U);
    t21 = *((char **)t20);
    memset(t18, 0, 8);
    t22 = (t18 + 4);
    t23 = (t21 + 4);
    t24 = *((unsigned int *)t21);
    t25 = (t24 >> 0);
    *((unsigned int *)t18) = t25;
    t26 = *((unsigned int *)t23);
    t27 = (t26 >> 0);
    *((unsigned int *)t22) = t27;
    t28 = *((unsigned int *)t18);
    *((unsigned int *)t18) = (t28 & 3U);
    t29 = *((unsigned int *)t22);
    *((unsigned int *)t22) = (t29 & 3U);
    t31 = (t1 + 6328);
    t32 = (t31 + 56U);
    t33 = *((char **)t32);
    memset(t30, 0, 8);
    t34 = (t30 + 4);
    t35 = (t33 + 4);
    t36 = *((unsigned int *)t33);
    t37 = (t36 >> 0);
    *((unsigned int *)t30) = t37;
    t38 = *((unsigned int *)t35);
    t39 = (t38 >> 0);
    *((unsigned int *)t34) = t39;
    t40 = *((unsigned int *)t30);
    *((unsigned int *)t30) = (t40 & 3U);
    t41 = *((unsigned int *)t34);
    *((unsigned int *)t34) = (t41 & 3U);
    t42 = ((char*)((ng6)));
    xsi_vlogtype_concat(t5, 8, 8, 4U, t42, 2, t30, 2, t18, 2, t6, 2);
    t43 = (t1 + 6808);
    xsi_vlogvar_assign_value(t43, t5, 0, 0, 8);
    xsi_set_current_line(125, ng0);
    t4 = (t1 + 6808);
    t7 = (t4 + 56U);
    t8 = *((char **)t7);
    t9 = (t2 + 56U);
    t10 = *((char **)t9);
    t11 = (t2 + 56U);
    t19 = *((char **)t11);
    xsi_vlog_subprograminvocation_setJumpstate(t2, t19, &&LAB6);
    t20 = (t2 + 56U);
    t21 = *((char **)t20);
    t22 = (t1 + 848);
    t23 = xsi_create_subprogram_invocation(t21, 0, t1, t22, 0, t2);
    xsi_vlog_subprogram_pushinvocation(t22, t23);
    t31 = (t1 + 5368);
    xsi_vlogvar_assign_value(t31, t8, 0, 0, 8);

LAB8:    t32 = (t2 + 64U);
    t33 = *((char **)t32);
    t34 = (t33 + 80U);
    t35 = *((char **)t34);
    t42 = (t35 + 272U);
    t43 = *((char **)t42);
    t44 = (t43 + 0U);
    t45 = *((char **)t44);
    t46 = ((int  (*)(char *, char *))t45)(t1, t33);
    if (t46 == -1)
        goto LAB9;

LAB10:    if (t46 != 0)
        goto LAB11;

LAB6:    t33 = (t1 + 848);
    xsi_vlog_subprogram_popinvocation(t33);

LAB7:    t47 = (t2 + 64U);
    t48 = *((char **)t47);
    t47 = (t1 + 848);
    t49 = (t2 + 56U);
    t50 = *((char **)t49);
    xsi_delete_subprogram_invocation(t47, t48, t1, t50, t2);

LAB4:    xsi_vlog_dispose_subprogram_invocation(t2);
    t4 = (t2 + 48U);
    *((char **)t4) = &&LAB2;
    t0 = 0;

LAB1:    return t0;
LAB9:    t0 = -1;
    goto LAB1;

LAB11:    t32 = (t2 + 48U);
    *((char **)t32) = &&LAB8;
    goto LAB1;

}

static int sp_tskRunMULT(char *t1, char *t2)
{
    char t5[8];
    char t6[8];
    char t18[8];
    char t30[8];
    int t0;
    char *t3;
    char *t4;
    char *t7;
    char *t8;
    char *t9;
    char *t10;
    char *t11;
    unsigned int t12;
    unsigned int t13;
    unsigned int t14;
    unsigned int t15;
    unsigned int t16;
    unsigned int t17;
    char *t19;
    char *t20;
    char *t21;
    char *t22;
    char *t23;
    unsigned int t24;
    unsigned int t25;
    unsigned int t26;
    unsigned int t27;
    unsigned int t28;
    unsigned int t29;
    char *t31;
    char *t32;
    char *t33;
    char *t34;
    char *t35;
    unsigned int t36;
    unsigned int t37;
    unsigned int t38;
    unsigned int t39;
    unsigned int t40;
    unsigned int t41;
    char *t42;
    char *t43;
    char *t44;
    char *t45;
    int t46;
    char *t47;
    char *t48;
    char *t49;
    char *t50;

LAB0:    t0 = 1;
    t3 = (t2 + 48U);
    t4 = *((char **)t3);
    if (t4 == 0)
        goto LAB2;

LAB3:    goto *t4;

LAB2:    t4 = (t1 + 2576);
    xsi_vlog_subprogram_setdisablestate(t4, &&LAB4);
    xsi_set_current_line(134, ng0);

LAB5:    xsi_set_current_line(135, ng0);
    t7 = (t1 + 7288);
    t8 = (t7 + 56U);
    t9 = *((char **)t8);
    memset(t6, 0, 8);
    t10 = (t6 + 4);
    t11 = (t9 + 4);
    t12 = *((unsigned int *)t9);
    t13 = (t12 >> 0);
    *((unsigned int *)t6) = t13;
    t14 = *((unsigned int *)t11);
    t15 = (t14 >> 0);
    *((unsigned int *)t10) = t15;
    t16 = *((unsigned int *)t6);
    *((unsigned int *)t6) = (t16 & 3U);
    t17 = *((unsigned int *)t10);
    *((unsigned int *)t10) = (t17 & 3U);
    t19 = (t1 + 7128);
    t20 = (t19 + 56U);
    t21 = *((char **)t20);
    memset(t18, 0, 8);
    t22 = (t18 + 4);
    t23 = (t21 + 4);
    t24 = *((unsigned int *)t21);
    t25 = (t24 >> 0);
    *((unsigned int *)t18) = t25;
    t26 = *((unsigned int *)t23);
    t27 = (t26 >> 0);
    *((unsigned int *)t22) = t27;
    t28 = *((unsigned int *)t18);
    *((unsigned int *)t18) = (t28 & 3U);
    t29 = *((unsigned int *)t22);
    *((unsigned int *)t22) = (t29 & 3U);
    t31 = (t1 + 6968);
    t32 = (t31 + 56U);
    t33 = *((char **)t32);
    memset(t30, 0, 8);
    t34 = (t30 + 4);
    t35 = (t33 + 4);
    t36 = *((unsigned int *)t33);
    t37 = (t36 >> 0);
    *((unsigned int *)t30) = t37;
    t38 = *((unsigned int *)t35);
    t39 = (t38 >> 0);
    *((unsigned int *)t34) = t39;
    t40 = *((unsigned int *)t30);
    *((unsigned int *)t30) = (t40 & 3U);
    t41 = *((unsigned int *)t34);
    *((unsigned int *)t34) = (t41 & 3U);
    t42 = ((char*)((ng7)));
    xsi_vlogtype_concat(t5, 8, 8, 4U, t42, 2, t30, 2, t18, 2, t6, 2);
    t43 = (t1 + 7448);
    xsi_vlogvar_assign_value(t43, t5, 0, 0, 8);
    xsi_set_current_line(136, ng0);
    t4 = (t1 + 7448);
    t7 = (t4 + 56U);
    t8 = *((char **)t7);
    t9 = (t2 + 56U);
    t10 = *((char **)t9);
    t11 = (t2 + 56U);
    t19 = *((char **)t11);
    xsi_vlog_subprograminvocation_setJumpstate(t2, t19, &&LAB6);
    t20 = (t2 + 56U);
    t21 = *((char **)t20);
    t22 = (t1 + 848);
    t23 = xsi_create_subprogram_invocation(t21, 0, t1, t22, 0, t2);
    xsi_vlog_subprogram_pushinvocation(t22, t23);
    t31 = (t1 + 5368);
    xsi_vlogvar_assign_value(t31, t8, 0, 0, 8);

LAB8:    t32 = (t2 + 64U);
    t33 = *((char **)t32);
    t34 = (t33 + 80U);
    t35 = *((char **)t34);
    t42 = (t35 + 272U);
    t43 = *((char **)t42);
    t44 = (t43 + 0U);
    t45 = *((char **)t44);
    t46 = ((int  (*)(char *, char *))t45)(t1, t33);
    if (t46 == -1)
        goto LAB9;

LAB10:    if (t46 != 0)
        goto LAB11;

LAB6:    t33 = (t1 + 848);
    xsi_vlog_subprogram_popinvocation(t33);

LAB7:    t47 = (t2 + 64U);
    t48 = *((char **)t47);
    t47 = (t1 + 848);
    t49 = (t2 + 56U);
    t50 = *((char **)t49);
    xsi_delete_subprogram_invocation(t47, t48, t1, t50, t2);

LAB4:    xsi_vlog_dispose_subprogram_invocation(t2);
    t4 = (t2 + 48U);
    *((char **)t4) = &&LAB2;
    t0 = 0;

LAB1:    return t0;
LAB9:    t0 = -1;
    goto LAB1;

LAB11:    t32 = (t2 + 48U);
    *((char **)t32) = &&LAB8;
    goto LAB1;

}

static void Initial_22_0(char *t0)
{
    char t5[8];
    char t9[8];
    char t32[8];
    char t58[8];
    char *t1;
    char *t2;
    char *t3;
    char *t4;
    char *t6;
    char *t7;
    char *t8;
    char *t10;
    char *t11;
    char *t12;
    unsigned int t13;
    unsigned int t14;
    unsigned int t15;
    int t16;
    char *t17;
    char *t18;
    char *t19;
    char *t20;
    char *t21;
    char *t22;
    unsigned int t23;
    unsigned int t24;
    unsigned int t25;
    unsigned int t26;
    unsigned int t27;
    unsigned int t28;
    unsigned int t29;
    unsigned int t30;
    unsigned int t31;
    char *t33;
    unsigned int t34;
    unsigned int t35;
    unsigned int t36;
    unsigned int t37;
    unsigned int t38;
    unsigned int t39;
    char *t40;
    char *t41;
    char *t42;
    char *t43;
    char *t44;
    char *t45;
    char *t46;
    char *t47;
    char *t48;
    char *t49;
    char *t50;
    char *t51;
    char *t52;
    int t53;
    char *t54;
    char *t55;
    char *t56;
    char *t57;
    char *t59;

LAB0:    t1 = (t0 + 8368U);
    t2 = *((char **)t1);
    if (t2 == 0)
        goto LAB2;

LAB3:    goto *t2;

LAB2:    xsi_set_current_line(23, ng0);

LAB4:    xsi_set_current_line(27, ng0);
    t2 = ((char*)((ng3)));
    t3 = (t0 + 4088);
    xsi_vlogvar_assign_value(t3, t2, 0, 0, 1);
    xsi_set_current_line(28, ng0);
    t2 = ((char*)((ng2)));
    t3 = (t0 + 4408);
    xsi_vlogvar_assign_value(t3, t2, 0, 0, 1);
    xsi_set_current_line(29, ng0);
    t2 = ((char*)((ng3)));
    t3 = (t0 + 4248);
    xsi_vlogvar_assign_value(t3, t2, 0, 0, 1);
    xsi_set_current_line(30, ng0);
    t2 = (t0 + 8176);
    xsi_process_wait(t2, 1000000LL);
    *((char **)t1) = &&LAB5;

LAB1:    return;
LAB5:    xsi_set_current_line(30, ng0);
    t3 = ((char*)((ng3)));
    t4 = (t0 + 4408);
    xsi_vlogvar_assign_value(t4, t3, 0, 0, 1);
    xsi_set_current_line(31, ng0);
    t2 = (t0 + 8176);
    xsi_process_wait(t2, 1500000000LL);
    *((char **)t1) = &&LAB6;
    goto LAB1;

LAB6:    xsi_set_current_line(34, ng0);
    *((int *)t5) = xsi_vlogfile_file_open_of_cname_ctype(ng8, ng9);
    t2 = (t5 + 4);
    *((int *)t2) = 0;
    t3 = (t0 + 4728);
    xsi_vlogvar_assign_value(t3, t5, 0, 0, 32);
    xsi_set_current_line(35, ng0);
    t2 = (t0 + 4728);
    t3 = (t2 + 56U);
    t4 = *((char **)t3);
    t6 = (t0 + 4888);
    t7 = (t6 + 56U);
    t8 = *((char **)t7);
    xsi_vlog_bit_copy(t5, 0, t8, 0, 32);
    *((int *)t9) = xsi_vlogfile_fscanf(*((unsigned int *)t4), ng10, 2, t0, (char)119, t5, 32);
    t10 = (t9 + 4);
    *((int *)t10) = 0;
    t11 = (t0 + 4888);
    xsi_vlogvar_assign_value(t11, t5, 0, 0, 32);
    t12 = (t0 + 5048);
    xsi_vlogvar_assign_value(t12, t9, 0, 0, 32);
    xsi_set_current_line(37, ng0);
    t2 = (t0 + 4888);
    t3 = (t2 + 56U);
    t4 = *((char **)t3);
    t6 = (t4 + 4);
    t13 = *((unsigned int *)t6);
    t14 = (~(t13));
    t15 = *((unsigned int *)t4);
    t16 = (t15 & t14);
    t7 = (t0 + 11628);
    *((int *)t7) = t16;

LAB7:    t8 = (t0 + 11628);
    if (*((int *)t8) > 0)
        goto LAB8;

LAB9:    xsi_set_current_line(48, ng0);
    t2 = (t0 + 4728);
    t3 = (t2 + 56U);
    t4 = *((char **)t3);
    xsi_vlogfile_fclose(*((unsigned int *)t4));
    xsi_set_current_line(62, ng0);
    t2 = (t0 + 8176);
    xsi_process_wait(t2, 1000000LL);
    *((char **)t1) = &&LAB45;
    goto LAB1;

LAB8:    xsi_set_current_line(38, ng0);

LAB10:    xsi_set_current_line(39, ng0);
    t10 = (t0 + 4728);
    t11 = (t10 + 56U);
    t12 = *((char **)t11);
    t17 = (t0 + 5208);
    t18 = (t17 + 56U);
    t19 = *((char **)t18);
    xsi_vlog_bit_copy(t5, 0, t19, 0, 8);
    *((int *)t9) = xsi_vlogfile_fscanf(*((unsigned int *)t12), ng10, 2, t0, (char)118, t5, 8);
    t20 = (t9 + 4);
    *((int *)t20) = 0;
    t21 = (t0 + 5208);
    xsi_vlogvar_assign_value(t21, t5, 0, 0, 8);
    t22 = (t0 + 5048);
    xsi_vlogvar_assign_value(t22, t9, 0, 0, 32);
    xsi_set_current_line(40, ng0);
    t2 = (t0 + 5208);
    t3 = (t2 + 56U);
    t4 = *((char **)t3);
    memset(t5, 0, 8);
    t6 = (t5 + 4);
    t7 = (t4 + 4);
    t13 = *((unsigned int *)t4);
    t14 = (t13 >> 6);
    *((unsigned int *)t5) = t14;
    t15 = *((unsigned int *)t7);
    t23 = (t15 >> 6);
    *((unsigned int *)t6) = t23;
    t24 = *((unsigned int *)t5);
    *((unsigned int *)t5) = (t24 & 3U);
    t25 = *((unsigned int *)t6);
    *((unsigned int *)t6) = (t25 & 3U);

LAB11:    t8 = ((char*)((ng4)));
    t16 = xsi_vlog_unsigned_case_compare(t5, 2, t8, 2);
    if (t16 == 1)
        goto LAB12;

LAB13:    t2 = ((char*)((ng6)));
    t16 = xsi_vlog_unsigned_case_compare(t5, 2, t2, 2);
    if (t16 == 1)
        goto LAB14;

LAB15:    t2 = ((char*)((ng7)));
    t16 = xsi_vlog_unsigned_case_compare(t5, 2, t2, 2);
    if (t16 == 1)
        goto LAB16;

LAB17:    t2 = ((char*)((ng5)));
    t16 = xsi_vlog_unsigned_case_compare(t5, 2, t2, 2);
    if (t16 == 1)
        goto LAB18;

LAB19:
LAB20:    t2 = (t0 + 11628);
    t16 = *((int *)t2);
    *((int *)t2) = (t16 - 1);
    goto LAB7;

LAB12:    xsi_set_current_line(41, ng0);
    t10 = (t0 + 5208);
    t11 = (t10 + 56U);
    t12 = *((char **)t11);
    memset(t9, 0, 8);
    t17 = (t9 + 4);
    t18 = (t12 + 4);
    t26 = *((unsigned int *)t12);
    t27 = (t26 >> 4);
    *((unsigned int *)t9) = t27;
    t28 = *((unsigned int *)t18);
    t29 = (t28 >> 4);
    *((unsigned int *)t17) = t29;
    t30 = *((unsigned int *)t9);
    *((unsigned int *)t9) = (t30 & 3U);
    t31 = *((unsigned int *)t17);
    *((unsigned int *)t17) = (t31 & 3U);
    t19 = (t0 + 5208);
    t20 = (t19 + 56U);
    t21 = *((char **)t20);
    memset(t32, 0, 8);
    t22 = (t32 + 4);
    t33 = (t21 + 4);
    t34 = *((unsigned int *)t21);
    t35 = (t34 >> 0);
    *((unsigned int *)t32) = t35;
    t36 = *((unsigned int *)t33);
    t37 = (t36 >> 0);
    *((unsigned int *)t22) = t37;
    t38 = *((unsigned int *)t32);
    *((unsigned int *)t32) = (t38 & 15U);
    t39 = *((unsigned int *)t22);
    *((unsigned int *)t22) = (t39 & 15U);
    t40 = (t0 + 8176);
    t41 = (t0 + 1280);
    t42 = xsi_create_subprogram_invocation(t40, 0, t0, t41, 0, 0);
    xsi_vlog_subprogram_pushinvocation(t41, t42);
    t43 = (t0 + 5528);
    xsi_vlogvar_assign_value(t43, t9, 0, 0, 2);
    t44 = (t0 + 5688);
    xsi_vlogvar_assign_value(t44, t32, 0, 0, 4);

LAB23:    t45 = (t0 + 8272);
    t46 = *((char **)t45);
    t47 = (t46 + 80U);
    t48 = *((char **)t47);
    t49 = (t48 + 272U);
    t50 = *((char **)t49);
    t51 = (t50 + 0U);
    t52 = *((char **)t51);
    t53 = ((int  (*)(char *, char *))t52)(t0, t46);

LAB25:    if (t53 != 0)
        goto LAB26;

LAB21:    t46 = (t0 + 1280);
    xsi_vlog_subprogram_popinvocation(t46);

LAB22:    t54 = (t0 + 8272);
    t55 = *((char **)t54);
    t54 = (t0 + 1280);
    t56 = (t0 + 8176);
    t57 = 0;
    xsi_delete_subprogram_invocation(t54, t55, t0, t56, t57);
    goto LAB20;

LAB14:    xsi_set_current_line(42, ng0);
    t3 = (t0 + 5208);
    t4 = (t3 + 56U);
    t6 = *((char **)t4);
    memset(t9, 0, 8);
    t7 = (t9 + 4);
    t8 = (t6 + 4);
    t13 = *((unsigned int *)t6);
    t14 = (t13 >> 4);
    *((unsigned int *)t9) = t14;
    t15 = *((unsigned int *)t8);
    t23 = (t15 >> 4);
    *((unsigned int *)t7) = t23;
    t24 = *((unsigned int *)t9);
    *((unsigned int *)t9) = (t24 & 3U);
    t25 = *((unsigned int *)t7);
    *((unsigned int *)t7) = (t25 & 3U);
    t10 = (t0 + 5208);
    t11 = (t10 + 56U);
    t12 = *((char **)t11);
    memset(t32, 0, 8);
    t17 = (t32 + 4);
    t18 = (t12 + 4);
    t26 = *((unsigned int *)t12);
    t27 = (t26 >> 2);
    *((unsigned int *)t32) = t27;
    t28 = *((unsigned int *)t18);
    t29 = (t28 >> 2);
    *((unsigned int *)t17) = t29;
    t30 = *((unsigned int *)t32);
    *((unsigned int *)t32) = (t30 & 3U);
    t31 = *((unsigned int *)t17);
    *((unsigned int *)t17) = (t31 & 3U);
    t19 = (t0 + 5208);
    t20 = (t19 + 56U);
    t21 = *((char **)t20);
    memset(t58, 0, 8);
    t22 = (t58 + 4);
    t33 = (t21 + 4);
    t34 = *((unsigned int *)t21);
    t35 = (t34 >> 0);
    *((unsigned int *)t58) = t35;
    t36 = *((unsigned int *)t33);
    t37 = (t36 >> 0);
    *((unsigned int *)t22) = t37;
    t38 = *((unsigned int *)t58);
    *((unsigned int *)t58) = (t38 & 3U);
    t39 = *((unsigned int *)t22);
    *((unsigned int *)t22) = (t39 & 3U);
    t40 = (t0 + 8176);
    t41 = (t0 + 2144);
    t42 = xsi_create_subprogram_invocation(t40, 0, t0, t41, 0, 0);
    xsi_vlog_subprogram_pushinvocation(t41, t42);
    t43 = (t0 + 6328);
    xsi_vlogvar_assign_value(t43, t9, 0, 0, 2);
    t44 = (t0 + 6488);
    xsi_vlogvar_assign_value(t44, t32, 0, 0, 2);
    t45 = (t0 + 6648);
    xsi_vlogvar_assign_value(t45, t58, 0, 0, 2);

LAB29:    t46 = (t0 + 8272);
    t47 = *((char **)t46);
    t48 = (t47 + 80U);
    t49 = *((char **)t48);
    t50 = (t49 + 272U);
    t51 = *((char **)t50);
    t52 = (t51 + 0U);
    t54 = *((char **)t52);
    t53 = ((int  (*)(char *, char *))t54)(t0, t47);

LAB31:    if (t53 != 0)
        goto LAB32;

LAB27:    t47 = (t0 + 2144);
    xsi_vlog_subprogram_popinvocation(t47);

LAB28:    t55 = (t0 + 8272);
    t56 = *((char **)t55);
    t55 = (t0 + 2144);
    t57 = (t0 + 8176);
    t59 = 0;
    xsi_delete_subprogram_invocation(t55, t56, t0, t57, t59);
    goto LAB20;

LAB16:    xsi_set_current_line(43, ng0);
    t3 = (t0 + 5208);
    t4 = (t3 + 56U);
    t6 = *((char **)t4);
    memset(t9, 0, 8);
    t7 = (t9 + 4);
    t8 = (t6 + 4);
    t13 = *((unsigned int *)t6);
    t14 = (t13 >> 4);
    *((unsigned int *)t9) = t14;
    t15 = *((unsigned int *)t8);
    t23 = (t15 >> 4);
    *((unsigned int *)t7) = t23;
    t24 = *((unsigned int *)t9);
    *((unsigned int *)t9) = (t24 & 3U);
    t25 = *((unsigned int *)t7);
    *((unsigned int *)t7) = (t25 & 3U);
    t10 = (t0 + 5208);
    t11 = (t10 + 56U);
    t12 = *((char **)t11);
    memset(t32, 0, 8);
    t17 = (t32 + 4);
    t18 = (t12 + 4);
    t26 = *((unsigned int *)t12);
    t27 = (t26 >> 2);
    *((unsigned int *)t32) = t27;
    t28 = *((unsigned int *)t18);
    t29 = (t28 >> 2);
    *((unsigned int *)t17) = t29;
    t30 = *((unsigned int *)t32);
    *((unsigned int *)t32) = (t30 & 3U);
    t31 = *((unsigned int *)t17);
    *((unsigned int *)t17) = (t31 & 3U);
    t19 = (t0 + 5208);
    t20 = (t19 + 56U);
    t21 = *((char **)t20);
    memset(t58, 0, 8);
    t22 = (t58 + 4);
    t33 = (t21 + 4);
    t34 = *((unsigned int *)t21);
    t35 = (t34 >> 0);
    *((unsigned int *)t58) = t35;
    t36 = *((unsigned int *)t33);
    t37 = (t36 >> 0);
    *((unsigned int *)t22) = t37;
    t38 = *((unsigned int *)t58);
    *((unsigned int *)t58) = (t38 & 3U);
    t39 = *((unsigned int *)t22);
    *((unsigned int *)t22) = (t39 & 3U);
    t40 = (t0 + 8176);
    t41 = (t0 + 2576);
    t42 = xsi_create_subprogram_invocation(t40, 0, t0, t41, 0, 0);
    xsi_vlog_subprogram_pushinvocation(t41, t42);
    t43 = (t0 + 6968);
    xsi_vlogvar_assign_value(t43, t9, 0, 0, 2);
    t44 = (t0 + 7128);
    xsi_vlogvar_assign_value(t44, t32, 0, 0, 2);
    t45 = (t0 + 7288);
    xsi_vlogvar_assign_value(t45, t58, 0, 0, 2);

LAB35:    t46 = (t0 + 8272);
    t47 = *((char **)t46);
    t48 = (t47 + 80U);
    t49 = *((char **)t48);
    t50 = (t49 + 272U);
    t51 = *((char **)t50);
    t52 = (t51 + 0U);
    t54 = *((char **)t52);
    t53 = ((int  (*)(char *, char *))t54)(t0, t47);

LAB37:    if (t53 != 0)
        goto LAB38;

LAB33:    t47 = (t0 + 2576);
    xsi_vlog_subprogram_popinvocation(t47);

LAB34:    t55 = (t0 + 8272);
    t56 = *((char **)t55);
    t55 = (t0 + 2576);
    t57 = (t0 + 8176);
    t59 = 0;
    xsi_delete_subprogram_invocation(t55, t56, t0, t57, t59);
    goto LAB20;

LAB18:    xsi_set_current_line(44, ng0);
    t3 = (t0 + 5208);
    t4 = (t3 + 56U);
    t6 = *((char **)t4);
    memset(t9, 0, 8);
    t7 = (t9 + 4);
    t8 = (t6 + 4);
    t13 = *((unsigned int *)t6);
    t14 = (t13 >> 4);
    *((unsigned int *)t9) = t14;
    t15 = *((unsigned int *)t8);
    t23 = (t15 >> 4);
    *((unsigned int *)t7) = t23;
    t24 = *((unsigned int *)t9);
    *((unsigned int *)t9) = (t24 & 3U);
    t25 = *((unsigned int *)t7);
    *((unsigned int *)t7) = (t25 & 3U);
    t10 = (t0 + 8176);
    t11 = (t0 + 1712);
    t12 = xsi_create_subprogram_invocation(t10, 0, t0, t11, 0, 0);
    xsi_vlog_subprogram_pushinvocation(t11, t12);
    t17 = (t0 + 6008);
    xsi_vlogvar_assign_value(t17, t9, 0, 0, 2);

LAB41:    t18 = (t0 + 8272);
    t19 = *((char **)t18);
    t20 = (t19 + 80U);
    t21 = *((char **)t20);
    t22 = (t21 + 272U);
    t33 = *((char **)t22);
    t40 = (t33 + 0U);
    t41 = *((char **)t40);
    t53 = ((int  (*)(char *, char *))t41)(t0, t19);

LAB43:    if (t53 != 0)
        goto LAB44;

LAB39:    t19 = (t0 + 1712);
    xsi_vlog_subprogram_popinvocation(t19);

LAB40:    t42 = (t0 + 8272);
    t43 = *((char **)t42);
    t42 = (t0 + 1712);
    t44 = (t0 + 8176);
    t45 = 0;
    xsi_delete_subprogram_invocation(t42, t43, t0, t44, t45);
    goto LAB20;

LAB24:;
LAB26:    t45 = (t0 + 8368U);
    *((char **)t45) = &&LAB23;
    goto LAB1;

LAB30:;
LAB32:    t46 = (t0 + 8368U);
    *((char **)t46) = &&LAB29;
    goto LAB1;

LAB36:;
LAB38:    t46 = (t0 + 8368U);
    *((char **)t46) = &&LAB35;
    goto LAB1;

LAB42:;
LAB44:    t18 = (t0 + 8368U);
    *((char **)t18) = &&LAB41;
    goto LAB1;

LAB45:    xsi_set_current_line(63, ng0);
    xsi_vlog_finish(1);
    goto LAB1;

}

static void Always_66_1(char *t0)
{
    char t3[8];
    char *t1;
    char *t2;
    char *t4;
    char *t5;
    char *t6;
    char *t7;
    unsigned int t8;
    unsigned int t9;
    unsigned int t10;
    unsigned int t11;
    unsigned int t12;
    char *t13;
    char *t14;
    char *t15;
    unsigned int t16;
    unsigned int t17;
    unsigned int t18;
    unsigned int t19;
    unsigned int t20;
    unsigned int t21;
    unsigned int t22;
    unsigned int t23;
    char *t24;

LAB0:    t1 = (t0 + 8616U);
    t2 = *((char **)t1);
    if (t2 == 0)
        goto LAB2;

LAB3:    goto *t2;

LAB2:    xsi_set_current_line(66, ng0);
    t2 = (t0 + 8424);
    xsi_process_wait(t2, 5000LL);
    *((char **)t1) = &&LAB4;

LAB1:    return;
LAB4:    xsi_set_current_line(66, ng0);
    t4 = (t0 + 4088);
    t5 = (t4 + 56U);
    t6 = *((char **)t5);
    memset(t3, 0, 8);
    t7 = (t6 + 4);
    t8 = *((unsigned int *)t7);
    t9 = (~(t8));
    t10 = *((unsigned int *)t6);
    t11 = (t10 & t9);
    t12 = (t11 & 1U);
    if (t12 != 0)
        goto LAB8;

LAB6:    if (*((unsigned int *)t7) == 0)
        goto LAB5;

LAB7:    t13 = (t3 + 4);
    *((unsigned int *)t3) = 1;
    *((unsigned int *)t13) = 1;

LAB8:    t14 = (t3 + 4);
    t15 = (t6 + 4);
    t16 = *((unsigned int *)t6);
    t17 = (~(t16));
    *((unsigned int *)t3) = t17;
    *((unsigned int *)t14) = 0;
    if (*((unsigned int *)t15) != 0)
        goto LAB10;

LAB9:    t22 = *((unsigned int *)t3);
    *((unsigned int *)t3) = (t22 & 1U);
    t23 = *((unsigned int *)t14);
    *((unsigned int *)t14) = (t23 & 1U);
    t24 = (t0 + 4088);
    xsi_vlogvar_assign_value(t24, t3, 0, 0, 1);
    goto LAB2;

LAB5:    *((unsigned int *)t3) = 1;
    goto LAB8;

LAB10:    t18 = *((unsigned int *)t3);
    t19 = *((unsigned int *)t15);
    *((unsigned int *)t3) = (t18 | t19);
    t20 = *((unsigned int *)t14);
    t21 = *((unsigned int *)t15);
    *((unsigned int *)t14) = (t20 | t21);
    goto LAB9;

}

static void Always_140_2(char *t0)
{
    char t14[8];
    char *t1;
    char *t2;
    char *t3;
    char *t4;
    char *t5;
    char *t6;
    char *t7;
    char *t8;
    unsigned int t9;
    unsigned int t10;
    unsigned int t11;
    unsigned int t12;
    unsigned int t13;
    char *t15;
    char *t16;
    char *t17;
    char *t18;
    char *t19;

LAB0:    t1 = (t0 + 8864U);
    t2 = *((char **)t1);
    if (t2 == 0)
        goto LAB2;

LAB3:    goto *t2;

LAB2:    xsi_set_current_line(140, ng0);
    t2 = (t0 + 9432);
    *((int *)t2) = 1;
    t3 = (t0 + 8896);
    *((char **)t3) = t2;
    *((char **)t1) = &&LAB4;

LAB1:    return;
LAB4:    xsi_set_current_line(141, ng0);
    t4 = (t0 + 11652);
    t5 = *((char **)t4);
    t6 = ((((char*)(t5))) + 56U);
    t7 = *((char **)t6);
    t8 = (t7 + 4);
    t9 = *((unsigned int *)t8);
    t10 = (~(t9));
    t11 = *((unsigned int *)t7);
    t12 = (t11 & t10);
    t13 = (t12 != 0);
    if (t13 > 0)
        goto LAB5;

LAB6:
LAB7:    goto LAB2;

LAB5:    xsi_set_current_line(142, ng0);
    *((int *)t14) = xsi_vlog_stime(1000.0000000000000, 1000.0000000000000);
    t15 = (t14 + 4);
    *((int *)t15) = 0;
    t16 = (t0 + 11676);
    t17 = *((char **)t16);
    t18 = ((((char*)(t17))) + 56U);
    t19 = *((char **)t18);
    xsi_vlogfile_write(1, 0, 0, ng11, 3, t0, (char)118, t14, 32, (char)118, t19, 8);
    goto LAB7;

}

static void Always_144_3(char *t0)
{
    char t4[8];
    char *t1;
    char *t2;
    char *t3;
    char *t5;
    char *t6;
    char *t7;

LAB0:    t1 = (t0 + 9112U);
    t2 = *((char **)t1);
    if (t2 == 0)
        goto LAB2;

LAB3:    goto *t2;

LAB2:    xsi_set_current_line(144, ng0);
    t2 = (t0 + 9448);
    *((int *)t2) = 1;
    t3 = (t0 + 9144);
    *((char **)t3) = t2;
    *((char **)t1) = &&LAB4;

LAB1:    return;
LAB4:    xsi_set_current_line(145, ng0);
    *((int *)t4) = xsi_vlog_stime(1000.0000000000000, 1000.0000000000000);
    t5 = (t4 + 4);
    *((int *)t5) = 0;
    t6 = (t0 + 3528U);
    t7 = *((char **)t6);
    xsi_vlogfile_write(1, 0, 0, ng12, 3, t0, (char)118, t4, 32, (char)118, t7, 8);
    goto LAB2;

}


extern void work_m_00000000003028795707_3671711236_init()
{
	static char *pe[] = {(void *)Initial_22_0,(void *)Always_66_1,(void *)Always_140_2,(void *)Always_144_3};
	static char *se[] = {(void *)sp_tskRunInst,(void *)sp_tskRunPUSH,(void *)sp_tskRunSEND,(void *)sp_tskRunADD,(void *)sp_tskRunMULT};
	xsi_register_didat("work_m_00000000003028795707_3671711236", "isim/tb_isim_beh.exe.sim/work/m_00000000003028795707_3671711236.didat");
	xsi_register_executes(pe);
	xsi_register_subprogram_executes(se);
}
