$PACKAGE APAP.REDOENQ
SUBROUTINE NOFILE.REDO.REL.RISK.GRP.LIMIT.AVAIL(COLL.ARR)
*------------------------------------------------------------------------------------------
*DESCRIPTION : This is a no file enquiry routine for the enquiry REDO.ENQ.REL.RISK.GRP.LIMIT.AVAIL
*------------------------------------------------------------------------------------------
*------------------------------------------------------------------------------------------
* * Input / Output
* --------------
* IN     : -NA-
* OUT    : -NA-
*------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : RENUGADEVI B
* PROGRAM NAME : NOFILE.REDO.REL.RISK.GRP.LIMIT.AVAIL
*------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE             WHO                REFERENCE         DESCRIPTION

* 24.06.2010      RENUGADEVI B      ODR-2009-10-0578   INITIAL CREATION

* 20.09.2010     SHANKAR RAJU       ODR-2010-09-0020   MODIFICATION FOR CR-010
*
*                                                      CR DESCRIPTION : Include a validation for the
*                                                                       field RELATION the valid values
*                                                                       are in the range 300 to 500
*  DATE             WHO                   REFERENCE 
* 06-APRIL-2023      Conversion Tool       R22 Auto Conversion  - VM to @VM , FM to @FM ,SM to @SM 
* 06-APRIL-2023      Harsha                R22 Manual Conversion - No changes 
*----------------------------------------------------------------------------
* -----------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.CUSTOMER
    $INSERT I_F.COLLATERAL
    $INSERT I_F.CREDIT.LIMIT.MAINTENANCE
    $INSERT I_F.REDO.RISK.GROUP
*
    GOSUB INIT
    GOSUB PROCESS
RETURN
*****
INIT:
*****
*
    COLL.DESC.INDV              = ''
    INDV.TOT.TAKEN              = ''
    EXEC.AVAIL.AMT              = ''
    COLL.DESC.GLOBAL            = ''
    REL.GLOBAL.VALUE            = ''
    REL.GLOBAL.LIMIT            = ''
    RISK.DESC.COLL              = ''
    TOT.TAKEN.COLL              = ''
    TOT.AVAIL.COLL              = ''
    RISK.DESC.WO.COLL           = ''
    TOT.TAKEN.WO.COLL           = ''
    TOT.AVAIL.WO.COLL           = ''

    FN.CUSTOMER                 = 'F.CUSTOMER'
    F.CUSTOMER                  = ''
    FN.COLLATERAL               = 'F.COLLATERAL'
    F.COLLATERAL                = ''
    FN.CREDIT.LIMIT.MAINTENANCE = 'F.CREDIT.LIMIT.MAINTENANCE'
    F.CREDIT.LIMIT.MAINTENANCE  = ''
    FN.COMPANY                  = 'F.COMPANY'
    F.COMPANY                   = ''
    FN.REDO.RISK.GROUP          = 'F.REDO.RISK.GROUP'
    F.REDO.RISK.GROUP           = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)
    CALL OPF(FN.COLLATERAL,F.COLLATERAL)
    CALL OPF(FN.CREDIT.LIMIT.MAINTENANCE,F.CREDIT.LIMIT.MAINTENANCE)
    CALL OPF(FN.COMPANY,F.COMPANY)
    CALL OPF(FN.REDO.RISK.GROUP,F.REDO.RISK.GROUP)

    LREF.APPL                   = 'CUSTOMER'
    LREF.FIELDS                 = 'L.CU.GRP.RIESGO'
    LREF.POS                    = ''
    CALL MULTI.GET.LOC.REF(LREF.APPL,LREF.FIELDS,LREF.POS)
    L.CU.GRP.RIESGO.POS         = LREF.POS<1,1>
RETURN
********
PROCESS:
********
    LOCATE "CUSTOMER.NO" IN D.FIELDS<1> SETTING CUS.POS THEN
        CUST.ID              = D.RANGE.AND.VALUE<CUS.POS>
    END
    R.CUST                   = '' ; CUST.ERR = ''
*
    CALL F.READ(FN.CUSTOMER,CUST.ID,R.CUSTOMER,F.CUSTOMER,CUST.ERR)
    IF R.CUSTOMER THEN
        RSK.GRP              = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.GRP.RIESGO.POS>
        REL.CUSTOMER         = R.CUSTOMER<EB.CUS.REL.CUSTOMER>
        REL.CODE.CUS         = R.CUSTOMER<EB.CUS.RELATION.CODE>
    END
    CHANGE @VM TO @FM IN REL.CODE.CUS
    LOOP
        REMOVE Y.CUS.ID FROM REL.CUSTOMER SETTING POS1
    WHILE Y.CUS.ID:POS1

        CUS.REL.CODE = REL.CODE.CUS<POS1>
******FOR CHANGE REQUEST CR-010*********field RELATION the valid values are in the range 300 to 500*******20/SEP/2010***

        IF CUS.REL.CODE GE 300 AND CUS.REL.CODE LE 500 THEN

            CALL F.READ(FN.CUSTOMER,Y.CUS.ID,R.REL.CUST,F.CUSTOMER,CUS.ERR1)
            IF R.REL.CUST THEN
                SECTOR   = R.REL.CUST<EB.CUS.SECTOR>
            END
            IF SECTOR EQ '1002' THEN
                GOSUB EXECUTIVE.EMP
            END ELSE
                GOSUB GLOBAL.LINKED
            END

        END

    REPEAT

*    COLL.DESC.INDV       = "GLOBAL LINKED"
*    COLL.DESC.GLOBAL     = "INDIVIDUAL EXECUTIVES & EMPLOYEES"
*    COLL.ARR<-1> = COLL.DESC.INDV:'*':INDV.TOT.TAKEN:'*':EXEC.AVAIL.AMT
*    COLL.ARR<-1> = COLL.DESC.GLOBAL:'*':REL.GLOBAL.VALUE:'*':REL.GLOBAL.LIMIT

    GOSUB RISK.GROUPS

RETURN
**************
EXECUTIVE.EMP:
**************
    SEL.CMD2 = "SELECT ":FN.COLLATERAL:" WITH @ID LIKE " :Y.CUS.ID: "..."
    CALL EB.READLIST(SEL.CMD2,SEL.LIST2,'',NOR2,SEL.ERR2)
    LOOP
        REMOVE Y.COLL.ID FROM SEL.LIST2 SETTING POS1
    WHILE Y.COLL.ID:POS1
        CALL F.READ(FN.COLLATERAL,Y.COLL.ID,R.COLLATERAL,F.COLLATERAL,COLL.ERR1)
        IF R.COLLATERAL THEN
            INDV.TOT.TAKEN       = R.COLLATERAL<COLL.NOMINAL.VALUE>
        END
        CALL CACHE.READ(FN.CREDIT.LIMIT.MAINTENANCE,'SYSTEM',R.CR.LIM.MAINT,CR.ERR1)
        IF R.CR.LIM.MAINT THEN
            TYPE.OF.GROUP = R.CR.LIM.MAINT<CRD.LIM.TYPE.OF.GROUP>
        END
        GROUP.CNT         = DCOUNT(TYPE.OF.GROUP,@VM)
        CNT2 = 1
        LOOP
        WHILE CNT2 LE GROUP.CNT
            IF R.CR.LIM.MAINT<CRD.LIM.TYPE.OF.GROUP,CNT2> EQ 'EXECUTIVES.AND.EMP.INDIVIDUAL.LIMIT' THEN
                EXEC.AVAIL.AMT    = R.CR.LIM.MAINT<CRD.LIM.AVAILABLE.AMOUNT,CNT2>
            END
            CNT2 +=1
        REPEAT
    REPEAT
RETURN
**************
GLOBAL.LINKED:
**************
    SEL.CMD2 = "SELECT ":FN.COLLATERAL:" WITH @ID LIKE " :Y.CUS.ID: "..."
    CALL EB.READLIST(SEL.CMD2,SEL.LIST2,'',NO1,SEL.ERR1)
    LOOP
        REMOVE Y.COLL.ID FROM SEL.LIST2 SETTING POS1
    WHILE Y.COLL.ID:POS1
        CALL F.READ(FN.COLLATERAL,Y.COLL.ID,R.COLLATERAL,F.COLLATERAL,COLL.ERR1)
        IF R.COLLATERAL THEN
            COLL.DESC.GLOBAL     = COLL.DESC
            REL.GLOBAL.VALUE     = R.COLLATERAL<COLL.NOMINAL.VALUE>
        END
        CALL CACHE.READ(FN.CREDIT.LIMIT.MAINTENANCE,'SYSTEM',R.CR.LIM.MAINT,CR.ERR1)
        IF R.CR.LIM.MAINT THEN
            TYPE.OF.GROUP           = R.CR.LIM.MAINT<CRD.LIM.TYPE.OF.GROUP>
        END
        GROUP.CNT                   = DCOUNT(TYPE.OF.GROUP,@VM)
        CNT2 = 1
        LOOP
        WHILE CNT2 LE GROUP.CNT
            IF R.CR.LIM.MAINT<CRD.LIM.TYPE.OF.GROUP,CNT2> EQ 'RELATED.GLOBAL.LIMIT' THEN
                REL.GLOBAL.LIMIT    = R.CR.LIM.MAINT<CRD.LIM.AVAILABLE.AMOUNT,CNT2>
            END
            CNT2 +=1
        REPEAT
    REPEAT
RETURN
************
RISK.GROUPS:
************
    RISK.GROUP.COUNT         = DCOUNT(RSK.GRP,@SM)
    CNT1 = 1
    LOOP
    WHILE CNT1 LE RISK.GROUP.COUNT
        RISK.ID = RSK.GRP<1,1,CNT1>
        CALL F.READ(FN.REDO.RISK.GROUP,RISK.ID,R.RISK.GROUP,F.REDO.RISK.GROUP,RISK.ERR)
        RISK.DESC          = R.RISK.GROUP<RG.GRP.SHORT.DESC>
        RISK.DESC.COLL     = RISK.DESC:"-":"WITH COLLATERAL"
        RISK.DESC.WO.COLL  = RISK.DESC:"-":"WITHOUT COLLATERAL"
        SEL.CMD1 = "SELECT ":FN.CUSTOMER:" WITH L.CU.GRP.RIESGO EQ " :RSK.GRP<1,1,CNT1>
        CALL EB.READLIST(SEL.CMD1,SEL.LIST1,'',NOR1,SEL.ERR1)
        LOOP
            REMOVE Y.CUS.ID FROM SEL.LIST1 SETTING POS1
        WHILE Y.CUS.ID:POS1
            SEL.CMD2 = "SELECT ":FN.COLLATERAL:" WITH @ID LIKE " :Y.CUS.ID: "..."

            CALL EB.READLIST(SEL.CMD2,SEL.LIST2,'',NOR1,SEL.ERR2)
            LOOP
                REMOVE Y.COLL.ID FROM SEL.LIST2 SETTING POS2
            WHILE Y.COLL.ID:POS2
                CALL F.READ(FN.COLLATERAL,Y.COLL.ID,R.COLLATERAL,F.COLLATERAL,COLL.ERR1)
                IF R.COLLATERAL THEN
                    COLL.DESC          = R.COLLATERAL<COLL.DESCRIPTION>
                    COLL.DESC.COLL     = COLL.DESC:"-":"WITH COLLATERAL"
                    COLL.DESC.WO.COLL  = COLL.DESC:"-":"WITHOUT COLLATERAL"
                    TOT.TAKEN.COLL     = R.COLLATERAL<COLL.EXECUTION.VALUE>
                    TOT.TAKEN.WO.COLL  = R.COLLATERAL<COLL.NOMINAL.VALUE> - R.COLLATERAL<COLL.EXECUTION.VALUE>
                    GOSUB FETCH.AVAILABLE.AMOUNT
                END
            REPEAT
        REPEAT

        COLL.DESC.INDV       = "GLOBAL LINKED"
        COLL.DESC.GLOBAL     = "INDIVIDUAL EXECUTIVES & EMPLOYEES"
        COLL.ARR<-1> = COLL.DESC.INDV:'*':INDV.TOT.TAKEN:'*':EXEC.AVAIL.AMT
        COLL.ARR<-1> = COLL.DESC.GLOBAL:'*':REL.GLOBAL.VALUE:'*':REL.GLOBAL.LIMIT

        COLL.ARR<-1> = RISK.DESC.COLL:'*':TOT.TAKEN.COLL:'*':TOT.AVAIL.COLL
        COLL.ARR<-1> = RISK.DESC.WO.COLL:'*':TOT.TAKEN.WO.COLL:'*':TOT.AVAIL.WO.COLL
        CNT1 +=1
    REPEAT
RETURN
************************
FETCH.AVAILABLE.AMOUNT:
************************
    CALL CACHE.READ(FN.CREDIT.LIMIT.MAINTENANCE,'SYSTEM',R.CR.LIM.MAINT,CR.ERR1)
    IF R.CR.LIM.MAINT THEN
        TYPE.OF.GROUP = R.CR.LIM.MAINT<CRD.LIM.TYPE.OF.GROUP>
    END
    GROUP.CNT         = DCOUNT(TYPE.OF.GROUP,@VM)
    CNT2 = 1
    LOOP
    WHILE CNT2 LE GROUP.CNT
        IF R.CR.LIM.MAINT<CRD.LIM.TYPE.OF.GROUP,CNT2> EQ 'RISK.GROUP.LIMIT.WITH.COLLATERAL' THEN
            TOT.AVAIL.COLL    = R.CR.LIM.MAINT<CRD.LIM.AVAILABLE.AMOUNT,CNT2>
        END
        IF R.CR.LIM.MAINT<CRD.LIM.TYPE.OF.GROUP,CNT2> EQ 'RISK.GROUP.LIMIT.WITHOUT.COLLATERAL' THEN
            TOT.AVAIL.WO.COLL = R.CR.LIM.MAINT<CRD.LIM.AVAILABLE.AMOUNT,CNT2>
        END
        CNT2 +=1
    REPEAT
RETURN
*--------------------------------------------------------------------------------------------------------------
END
