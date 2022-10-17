select
    c.class_id,
    c.class_desc,
    c.dmn_id,
    c.dmn_desc,
    c.qual_id,
    c.qual_title,
    c.start_dte,
    c.end_dte,
    c.max_size,
    o.col_num,
    o.label,
    o.user_value,
    o.user_desc,
    s.stud_id,
    s.class_stat_id,
    s.class_stat_desc,
    s.comments,
    s.lname,
    s.fname,
    s.mi,
    sc.schd_id,
    sc.cpnt_typ_id,
    sc.act_cpnt_id,
    sc.rev_dte,
    sc.timezone_id,
    sc.display_in_schd_tz,
    sc.rev_num,
    sc.start_dte,
    sc.end_dte
from
    (
        select
            pa_class.class_id,
            pa_class.class_desc,
            pa_class.dmn_id,
            pa_domain.dmn_desc,
            pa_class.qual_id,
            pa_qual.qual_title,
            pa_class.start_dte,
            pa_class.end_dte,
            pa_class.max_size
        from
            pa_class
            left join pa_domain on pa_class.dmn_id = pa_domain.dmn_id
            left join pa_qual on pa_class.qual_id = pa_qual.qual_id
    ) as c
    left join (
        select
            pa_class_user.class_id,
            pa_class_user.col_num,
            pa_class_user.user_value,
            pa_usrcl_class.label,
            pa_usrrf_class.user_desc
        from
            pa_class_user
            left join pa_usrcl_class on (
                pa_class_user.col_num = pa_usrcl_class.col_num
            )
            left join pa_usrrf_class on (
                pa_class_user.user_value = pa_usrrf_class.user_id
                and pa_class_user.col_num = pa_usrrf_class.col_num
            )
    ) as o on c.class_id = o.class_id
    left join (
        select
            pa_class_student.class_id,
            pa_class_student.stud_id,
            pa_class_student.class_stat_id,
            pa_class_stat.class_stat_desc,
            pa_class_student.comments,
            pa_student.lname,
            pa_student.fname,
            pa_student.mi
        from
            pa_class_student
            left join pa_class_stat on pa_class_student.class_stat_id = pa_class_stat.class_stat_id
            left join pa_student on pa_class_student.stud_id = pa_student.stud_id
    ) as s on c.class_id = s.class_id
    left join (
        select
            pa_class_sched.class_id,
            pa_class_sched.schd_id,
            pa_sched.cpnt_typ_id,
            pa_sched.act_cpnt_id,
            pa_sched.rev_dte,
            pa_sched.timezone_id,
            pa_sched.display_in_schd_tz,
            pv_course.rev_num,
            ps_schd_resources.start_dte,
            ps_schd_resources.end_dte
        from
            pa_class_sched
            left join pa_sched on pa_class_sched.schd_id = pa_sched.schd_id
            left join pv_course on (
                pa_sched.cpnt_typ_id = pv_course.cpnt_typ_id
                and pa_sched.act_cpnt_id = pv_course.act_cpnt_id
                and pa_sched.rev_dte = pv_course.rev_dte
            )
            left join ps_schd_resources on pa_class_sched.schd_id = ps_schd_resources.schd_id
    ) as sc on c.class_id = sc.class_id