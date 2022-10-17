TARGET_QUERY (
  "Get data of classes, students and schedules"
  rm_target = RM (
    rm1 = RM(
      "Get data of class"
      mq1_1 = MQ (
        PA_CLASS
        CLASS_ID,
        CLASS_DESC,
        DMN_ID,
        QUAL_ID,
        START_DTE,
        END_DTE,
        MAX_SIZE
      )
      mq1_2 = MQ (
        PA_DOMAIN
        DMN_ID AS di,
        DMN_DESC
      )
      mq1_3 = MQ(
        PA_QUAL
        QUAL_ID AS qi,
        QUAL_TITLE
      )
      mq1_4 = MQ(
        PA_CLASS_USER
        CLASS_ID AS ci,
        COL_NUM,
        USER_VALUE
      )
      
      mq1_5 = MQ(
        PA_USRCL_CLASS
        COL_NUM as cn,
        LABEL
      )
      
      mq1_6 = MQ(
        PA_USRRF_CLASS
        COL_NUM AS cn2,
        USER_ID AS ui,
        USER_DESC
      )
      
      mq1_1 -> (1, DMN_ID=di) -> mq1_2
      mq1_1 -> (1, QUAL_ID=qi) -> mq1_3
      mq1_1 -> (n, CLASS_ID=ci) -> mq1_4
      mq1_4 -> (1, COL_NUM=cn) -> mq1_5
      mq1_4 -> (1, COL_NUM=cn2, USER_VALUE=ui) -> mq1_6
    )
  
    rm2 = RM(
      "Get data of students"
      mq2_1 = MQ(
        PA_CLASS_STUDENT
        CLASS_ID AS ci2,
        STUD_ID,
        CLASS_STAT_ID,
        COMMENTS
      )
      
      mq2_2 = MQ(
        PA_STUDENT
        STUD_ID AS si,
        LNAME,
        FNAME,
        MI
      )
      
      mq2_3 = MQ(
        PA_CLASS_STAT
        CLASS_STAT_ID as csi,
        CLASS_STAT_DESC
      )
      
      mq2_1 -> (1, STUD_ID=si) -> mq2_2
      mq2_1 -> (1, CLASS_STAT_ID=csi) -> mq2_3
    )
  
    rm3 = RM(
      "Get data of schedules"
      mq3_1 = MQ(
        PA_CLASS_SCHED
        CLASS_ID AS ci3,
        SCHD_ID
      )
      
      mq3_2 = MQ(
        PA_SCHED
        SCHD_ID AS si2,
        CPNT_TYP_ID,
        ACT_CPNT_ID,
        REV_DTE,
        TIMEZONE_ID,
        DISPLAY_IN_SCHD_TZ
      )
      
      mq3_3 = MQ(
        PV_COURSE
        CPNT_TYP_ID AS cti,
        ACT_CPNT_ID AS api,
        REV_DTE AS rd,
        REV_NUM
      )
      
      mq3_4 = MQ(
        PS_SCHD_RESOURCES
        SCHD_ID AS si3,
        START_DTE,
        END_DTE
      )
      
      mq3_1 -> (1, SCHD_ID=si2) -> mq3_2
      mq3_1 -> (1, SCHD_ID=si3) -> mq3_4
      mq3_2 -> (1, CPNT_TYP_ID=cti, ACT_CPNT_ID=aci, REV_DTE=rd) -> mq3_3
    )
  
    rm1 -> (n, CLASS_ID=ci2) -> rm2
    rm1 -> (n, CLASS_ID=ci3) -> rm3
  )

  mq_last = MQ (
    rm_target
    CLASS_ID,
    CLASS_DESC,
    DMN_ID,
    DMN_DESC,
    QUAL_ID,
    QUAL_TITLE,
    START_DTE,
    END_DTE,
    MAX_SIZE,
    COL_NUM,
    USER_VALUE,
    LABEL,
    USER_DESC,
    STUD_ID,
    LNAME,
    FNAME,
    MI, 
    CLASS_STAT_ID,
    CLASS_STAT_DESC,
    COMMENTS,
    SCHD_ID,
    START_DTE,
    END_DTE,
    CPNT_TYP_ID,
    ACT_CPNT_ID,
    REV_DTE,
    TIMEZONE_ID,
    DISPLAY_IN_SCHD_TZ,
    REV_NUM
  )  
)
