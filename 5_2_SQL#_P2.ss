TARGET_QUERY (
  "Get class information"
  rm_summary = RM (
    rm1 = RM (
      "Get class information"
      mq11 = MQ (
        PA_CLASS
        CLASS_ID,
        CLASS_DESC,
        DMN_ID,
        QUAL_ID,
        START_DTE,
        END_DTE,
        MAX_SIZE
    )
    mq12 = MQ (
      PA_DOMAIN
      DMN_ID AS di,
      DMN_DESC
    )
    mq13 = MQ (
      PA_QUAL
      QUAL_ID AS qi,
      QUAL_TITLE
    )
    mq14 = MQ (
      PA_CLASS_USER
      CLASS_ID AS ci,
      COL_NUM,
      USER_VALUE
    )
    mq15 = MQ (
      PA_USRCL_CLASS
      COL_NUM AS cn,
      LABEL
    )
    mq16 = MQ (
      PA_USRRF_CLASS
      COL_NUM AS cn2,
      USER_ID AS ui,
      USER_DESC
    )
    mq11 -> (1, DMN_ID = di) -> mq12
    mq11 -> (1, QUAL_ID = qi) -> mq13
    mq11 -> (1, CLASS_ID = ci) -> mq14
    mq14 -> (1, COL_NUM = cn) -> mq15
    mq14 -> (1, COL_NUM = cn2,USER_VALUE = ui) -> mq16
    )
  
    rm2 = RM (
      "Get class student information"
      mq21 = MQ (
        PA_CLASS_STUDENT
        CLASS_ID AS ci2,
        STUD_ID,
        CLASS_STAT_ID,
        COMMENTS
      )
      mq22 = MQ (
        PA_STUDENT
        STUD_ID AS si,
        LNAME,
        FNAME,
        MI
      )
      mq23 = MQ (
        PA_CLASS_STAT
        CLASS_STAT_ID AS csi,
        CLASS_STAT_DESC
      )
      mq21 -> (1, STUD_ID = si) -> mq22
      mq21 -> (1, CLASS_STAT_ID = csi) -> mq23
    )
  
    rm3 = RM (
      "Get class schedule information"
      mq31 = MQ (
        PA_CLASS_SCHED
        CLASS_ID AS ci3,
        SCHD_ID
      )
      mq32 = MQ (
        PA_SCHED
        SCHD_ID AS si2,
        CPNT_TYP_ID,
        ACT_CPNT_ID,
        REV_DTE,
        TIMEZONE_ID,
        DISPLAY_IN_SCHD_TZ
     )
      mq33 = MQ (
        PV_COURSE
        CPNT_TYP_ID AS cti,
        ACT_CPNT_ID AS aci,
        REV_DTE AS rd,
        REV_NUM
      )
      mq34 = MQ (
        PS_SCHD_RESOURCES
        SCHD_ID AS si3,
        START_DTE,
        END_DTE
      )
       mq31 -> (1, SCHD_ID = si2) -> mq32
       mq32 -> (1, CPNT_TYP_ID = cti,ACT_CPNT_ID = aci,REV_DTE = rd) -> mq33
       mq31 -> (1, SCHD_ID = si3) -> mq34
    )
    rm1 -> (n, CLASS_ID = ci2) -> rm2
    rm1 -> (n, CLASS_ID = ci3) -> rm3
  )
  mq_target = MQ (
    rm_summary
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

