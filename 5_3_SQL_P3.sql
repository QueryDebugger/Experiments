SELECT
    PA_CLASS.CLASS_ID AS "ClassId",
    PA_CLASS.CLASS_DESC AS "ClassDesc",
    PA_CLASS.DMN_ID AS "DmnId",
    PA_CLASS.QUAL_ID AS "QualId",
    PA_CLASS.START_DTE AS "StartDte",
    PA_CLASS.END_DTE AS "EndDte",
    PA_CLASS.MAX_SIZE AS "MaxSize",
    PA_DOMAIN.DMN_DESC AS "DmnDesc",
    PA_QUAL.QUAL_TITLE AS "QualTitle",
    PA_USRCL_CLASS.COL_NUM AS "ColNum",
    PA_USRCL_CLASS.LABEL AS "Label",
    PA_CLASS_USER.USER_VALUE AS "UserValue",
    PA_USRRF_CLASS.USER_DESC AS "UserDesc",
    PA_CLASS_STUDENT.STUD_ID AS "StudId",
    PA_CLASS_STUDENT.CLASS_STAT_ID AS "ClassStatId",
    PA_CLASS_STUDENT.COMMENTS AS "Comments",
    PA_STUDENT.LNAME AS "Lname",
    PA_STUDENT.FNAME AS "Fname",
    PA_STUDENT.MI AS "Mi",
    PA_CLASS_STAT.CLASS_STAT_DESC AS "ClassStatDesc",
    PA_CLASS_SCHED.SCHD_ID AS "SchdId",
    PA_CLASS_SCHED.CPNT_TYP_ID AS "CpntTypId",
    PA_CLASS_SCHED.ACT_CPNT_ID AS "ActCpntId",
    PA_CLASS_SCHED.REV_DTE AS "RevDte",
    PA_CLASS_SCHED.TIMEZONE_ID AS "TimezoneId",
    PA_CLASS_SCHED.DISPLAY_IN_SCHD_TZ AS "DisplayInSchdTz",
    PV_COURSE.DISPLAY_IN_SCHD_TZ AS "RevNum",
    PS_SCHD_RESOURCES.START_DTE AS "StartDte",
    PS_SCHD_RESOURCES.END_DTE AS "EndDte"
FROM
    PA_CLASS
    LEFT JOIN PA_DOMAIN ON PA_DOMAIN.DMN_ID = PA_CLASS.DMN_ID
    LEFT JOIN PA_QUAL ON PA_QUAL.QUAL_ID = PA_CLASS.QUAL_ID
    LEFT JOIN PA_CLASS_USER ON PA_CLASS_USER.CLASS_ID = PA_CLASS.CLASS_ID
    LEFT JOIN PA_USRCL_CLASS ON PA_USRCL_CLASS.COL_NUM = PA_CLASS_USER.COL_NUM
    LEFT JOIN PA_USRRF_CLASS ON ( 
        PA_USRRF_CLASS.USER_ID = PA_CLASS_USER.USER_VALUE
        AND PA_USRRF_CLASS.COL_NUM = PA_CLASS_USER.COL_NUM
    )
    LEFT JOIN PA_CLASS_STUDENT ON PA_CLASS_STUDENT.CLASS_ID = PA_CLASS.CLASS_ID
    LEFT JOIN PA_STUDENT ON PA_STUDENT.STUD_ID = PA_CLASS_STUDENT.STUD_ID
    LEFT JOIN PA_CLASS_STAT ON PA_CLASS_STAT.CLASS_STAT_ID = PA_CLASS_STUDENT.CLASS_STAT_ID
    LEFT JOIN PA_CLASS_SCHED ON PA_CLASS_SCHED.CLASS_ID = PA_CLASS.CLASS_ID
    LEFT JOIN PA_SCHED ON PA_SCHED.SCHD_ID = PA_CLASS_SCHED.SCHD_ID
    LEFT JOIN PV_COURSE ON (
        PV_COURSE.CPNT_TYP_ID = PA_CLASS_SCHED.CPNT_TYP_ID
        AND PV_COURSE.ACT_CPNT_ID = PA_CLASS_SCHED.ACT_CPNT_ID
        AND PV_COURSE.REV_DTE = PA_CLASS_SCHED.REV_DTE
    )
    LEFT JOIN PS_SCHD_RESOURCES ON PS_SCHD_RESOURCES.SCHD_ID = PA_CLASS_SCHED.SCHD_ID