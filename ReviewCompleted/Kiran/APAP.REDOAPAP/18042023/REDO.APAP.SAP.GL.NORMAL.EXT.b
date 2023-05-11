* @ValidationCode : MjoxMDc2OTQxNTg1OkNwMTI1MjoxNjgxODA1NDgxOTE3OmFqaXRoOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 18 Apr 2023 13:41:21
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ajith
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOAPAP
SUBROUTINE REDO.APAP.SAP.GL.NORMAL.EXT(GIT.MAP.VALUE,CONV.ERR)
*--------------------------------------------------------------------------------------------------------
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By      : Temenos Application Management
*Program   Name    : REDO.APAP.SAP.GL.NORMAL.EXT
*--------------------------------------------------------------------------------------------------------
*Description  :This routine is used to get detail report of all the transactions for the given day
*Linked With  : GIT.INTERFACE.OUT id SAP.NORMAL.EXTRACT
*In Parameter : GIT.MAP.VALUE -- contains the RE.STAT.LINE.BAL id
*Out Parameter: GIT.MAP.VALUE -- contains the list of values which are to be passsed to the out file
*               CONV.ERR      -- contains the error message
*--------------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
*    Date            Who                  Reference               Description
*   ------         ------               -------------            -------------
* 25 OCT 2010    Mohammed Anies K      ODR-2009-12-0294 C.12         Initial Creation
* 08 JUL 2010    Prabhu N              PACS00032519                  LCY Calculation added
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*18-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION  NO CHANGE
*18-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE
*----------------------------------------------------------------------------------------
*--------------------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.RE.STAT.LINE.BAL
    $INSERT I_F.RE.STAT.REP.LINE
*   $INSERT I_F.RE.STAT.REP.LINE
    $INSERT I_F.RE.STAT.LINE.CONT
    $INSERT I_F.DATES
    $INSERT I_F.COMPANY
    $INSERT I_GIT.COMMON
    $INSERT I_GIT.ONLINE.VAR
    $INSERT I_F.REDO.GL.H.EXTRACT.PARAMETER
    $INSERT I_F.REDO.GL.W.EXTRACT.ONLINE
    $INSERT I_F.REDO.INTRF.REP.LINE
    $INSERT I_F.REDO.CAPL.L.RE.STAT.LINE.CONT
    $INSERT I_REDO.B.SAP.VAL.COMMON
*--------------------------------------------------------------------------------------------------------
*********
MAIN.PARA:
*********

    GOSUB INITIALISE
    GOSUB GET.PARAM.DETAILS
    GOSUB PROCESS.PARA
    GIT.MAP.VALUE = Y.FT.OUT.LIST
RETURN
*--------------------------------------------------------------------------------------------------------
***********
INITIALISE:
***********

    Y.FLD.DELIM = '*'
    Y.REC.DELIM = '#'
    Y.RE.STAT.LINE.BAL.ID = GIT.MAP.VALUE
    Y.FT.OUT.LIST = ''
RETURN

*--------------------------------------------------------------------------------------------------------
*****************
GET.PARAM.DETAILS:
*****************
    IF R.REDO.GL.H.EXTRACT.PARAMETER THEN

        Y.GIT.NAME.LIST = R.REDO.GL.H.EXTRACT.PARAMETER<SAP.EP.GIT.ROUTINE>
        LOCATE GIT.COM.OUT.INT.ID IN Y.GIT.NAME.LIST<1,1> SETTING Y.GIT.NAME.POS THEN

            Y.PARAM.REPORT.AL = R.REDO.GL.H.EXTRACT.PARAMETER<SAP.EP.REPORT.AL,Y.GIT.NAME.POS>
            Y.PARAM.REPORT.PL = R.REDO.GL.H.EXTRACT.PARAMETER<SAP.EP.REPORT.PL,Y.GIT.NAME.POS>
            DESCRIPTION.POS = R.REDO.GL.H.EXTRACT.PARAMETER<SAP.EP.GL.DESCRIPTION,Y.GIT.NAME.POS>
            N.GL.IND.POS = R.REDO.GL.H.EXTRACT.PARAMETER<SAP.EP.N.GL.IND,Y.GIT.NAME.POS>
            Y.PARAM.CREDIT.FORMAT = R.REDO.GL.H.EXTRACT.PARAMETER<SAP.EP.CREDIT.FORMAT,Y.GIT.NAME.POS>
            Y.PARAM.DEBIT.FORMAT =  R.REDO.GL.H.EXTRACT.PARAMETER<SAP.EP.DEBIT.FORMAT,Y.GIT.NAME.POS>

            Y.PARAM.LINE.BAL = R.REDO.GL.H.EXTRACT.PARAMETER<SAP.EP.TYPE.OF.EXTRACT,Y.GIT.NAME.POS>
            Y.DB.TYPE.OF.TXN = R.REDO.GL.H.EXTRACT.PARAMETER<SAP.EP.EXT.DEBIT.CODE>
            Y.CR.TYPE.OF.TXN = R.REDO.GL.H.EXTRACT.PARAMETER<SAP.EP.EXT.CREDIT.CODE>
        END

    END
    EXT.DATE = R.REDO.GL.W.EXTRACT.ONLINE<SAP.GL.EO.ACTION.DATE>

    GOSUB GET.SAP.ACCOUNT.NUMBER
RETURN

*--------------------------------------------------------------------------------------------------------
************
PROCESS.PARA:
************

    IF RUNNING.UNDER.BATCH THEN
        PROCESS.DATE = R.DATES(EB.DAT.LAST.WORKING.DAY)
    END ELSE
        PROCESS.DATE = EXT.DATE
        IF PROCESS.DATE EQ "" THEN
            PROCESS.DATE = R.DATES(EB.DAT.LAST.WORKING.DAY)
        END
    END

    Y.RE.STAT.LINE.BAL.REPORT = FIELD(Y.RE.STAT.LINE.BAL.ID, "-", 1)
    Y.RE.STAT.LINE.BAL.ACCT = FIELD(Y.RE.STAT.LINE.BAL.ID, "-", 2)
    Y.RE.STAT.LINE.BAL.CUR = FIELD(Y.RE.STAT.LINE.BAL.ID,"-",3)
    Y.RE.STAT.LINE.BAL.DATE = FIELD(Y.RE.STAT.LINE.BAL.ID,"-",4)
    Y.CLOSE.DATE  = FIELD(Y.RE.STAT.LINE.BAL.DATE,"*",1)
    Y.LINE.CONT.COM = FIELD(Y.RE.STAT.LINE.BAL.ID,"*",2,1)
*  CALL F.READ(FN.COMPANY,Y.LINE.CONT.COM,R.COMPANY.ACC,F.COMPANY,ERR)  ;* TUS Start
    CALL CACHE.READ(FN.COMPANY,Y.LINE.CONT.COM,R.COMPANY.ACC,ERR)         ;* TUS End
    Y.LINE.CONT.COM=R.COMPANY.ACC<EB.COM.LOCAL.REF><1,LOC.EXT.GL.CC.POS>
    Y.SAP.ACCT.COMP=Y.LINE.CONT.COM
    Y.REDO.INTRF.REP.LINE.ID = Y.RE.STAT.LINE.BAL.REPORT:'.':Y.RE.STAT.LINE.BAL.ACCT
    CALL F.READ(FN.REDO.INTRF.REP.LINE,Y.REDO.INTRF.REP.LINE.ID,R.REDO.INTRF.REP.LINE,F.REDO.INTRF.REP.LINE,REDO.INTRF.REP.LINE.ERR)
    IF NOT(R.REDO.INTRF.REP.LINE) THEN
        RETURN
    END
    Y.INTRF.LINE.BAL = R.REDO.INTRF.REP.LINE<SAP.INTRF.LINE.BALANCE>
    Y.INTRF.AL.PL = R.REDO.INTRF.REP.LINE<SAP.INTRF.AL.PL>
    DESCRIP = R.REDO.INTRF.REP.LINE<SAP.INTRF.DESC,DESCRIPTION.POS>
    Y.IS.CONTINGENT = R.REDO.INTRF.REP.LINE<SAP.INTRF.IS.CONTINGENT>
    IF Y.IS.CONTINGENT THEN
        Y.IS.CONTINGENT = 'CT'
    END ELSE
        Y.IS.CONTINGENT = 'NCT'
    END
    GOSUB TOTAL.AMOUNT.PROCESS

RETURN
*--------------------------------------------------------------------------------------------------------
**********************
GET.SAP.ACCOUNT.NUMBER:
**********************
    ADDRESS.ID = Y.RE.STAT.LINE.BAL.ID
    ANS = INDEX(ADDRESS.ID, "-", 2)
    RE.STAT.REP.LINE.ID = ADDRESS.ID[1,ANS-1]
    SOL2 = INDEX(RE.STAT.REP.LINE.ID, "-", 1)
    SOL3 = RE.STAT.REP.LINE.ID[SOL2,1]
    SOL3 = "."
    RE.STAT.REP.LINE.ID[SOL2,1] = SOL3
    RE.STAT.REP.ID = RE.STAT.REP.LINE.ID

    CALL F.READ(FN.RE.STAT.REP.LINE,RE.STAT.REP.ID,R.STAT.REP.LINE,F.RE.STAT.REP.LINE,STAT.REP.LINE.ERR)
    Y.SAP.ACC.NO = R.STAT.REP.LINE<RE.SRL.DESC,N.GL.IND.POS>
*    Y.SAP.ACCT.COMP=R.STAT.REP.LINE<RE.SRL.CO.CODE>
*    CALL F.READ(FN.COMPANY,Y.SAP.ACCT.COMP,R.COMPANY.ACC,F.COMPANY,ERR)
*    Y.SAP.ACCT.COMP=R.COMPANY.ACC<EB.COM.LOCAL.REF><1,LOC.EXT.GL.CC.POS>
RETURN
*--------------------------------------------------------------------------------------------------------
********************
TOTAL.AMOUNT.PROCESS:
********************

    CALL F.READ(FN.RE.STAT.LINE.BAL,Y.RE.STAT.LINE.BAL.ID,R.RE.STAT.LINE.BAL,F.RE.STAT.LINE.BAL,RE.STAT.LINE.BAL.ERR)
    IF NOT(R.RE.STAT.LINE.BAL) THEN
        RETURN
    END

    Y.CR.MOVEMENT = 0
    Y.DB.MOVEMENT = 0
    Y.CR.MVT.LCY  = 0
    Y.DB.MVT.LCY  = 0

    Y.CR.MOVEMENT = R.RE.STAT.LINE.BAL<RE.SLB.CR.MOVEMENT> + 0
    Y.DB.MOVEMENT = R.RE.STAT.LINE.BAL<RE.SLB.DB.MOVEMENT> + 0
    Y.CR.MVT.LCY = R.RE.STAT.LINE.BAL<RE.SLB.CR.MVMT.LCL>  + 0
    Y.DB.MVT.LCY = R.RE.STAT.LINE.BAL<RE.SLB.DB.MVMT.LCL>  + 0

    IF Y.PARAM.CREDIT.FORMAT EQ '-' THEN
        Y.CR.MVT.LCY = (-1) * Y.CR.MVT.LCY
        Y.CR.MOVEMENT = (-1) * Y.CR.MOVEMENT
    END

    IF Y.PARAM.DEBIT.FORMAT EQ '+' THEN
        Y.DB.MVT.LCY = (-1) * Y.DB.MVT.LCY
        Y.DB.MOVEMENT = (-1) * Y.DB.MOVEMENT
    END
    Y.CR.MOVEMENT=FMT(Y.CR.MOVEMENT,2)
    Y.DB.MOVEMENT=FMT(Y.DB.MOVEMENT,2)
    Y.CR.MVT.LCY=FMT(Y.CR.MVT.LCY,2)
    Y.DB.MVT.LCY=FMT(Y.DB.MVT.LCY,2)
    IF Y.CR.MOVEMENT NE 0 THEN

        GOSUB CR.MOVEMENT.AL
    END
    IF Y.DB.MOVEMENT NE 0 THEN

        GOSUB  DB.MOVEMENT.AL
    END
RETURN

*--------------
CR.MOVEMENT.AL:
*--------------
    BEGIN CASE

        CASE Y.INTRF.AL.PL EQ 'AL'
            BEGIN CASE

                CASE Y.PARAM.REPORT.AL EQ 'LCY'
*PACS00032519-S
                    Y.FT.OUT.LIST := Y.REC.DELIM
                    IF Y.RE.STAT.LINE.BAL.CUR EQ LCCY THEN
*!!!!!!!
                        GOSUB FMT.CR.MVMT.LCY
*!!!!!!!
                        Y.FT.OUT.LIST := Y.CR.TYPE.OF.TXN:'*':Y.SAP.ACC.NO:'*':Y.LINE.CONT.COM:'*':Y.RE.STAT.LINE.BAL.CUR:'*':Y.CR.MVT.LCY:'*':'0':'*':Y.IS.CONTINGENT:'*':Y.SAP.ACCT.COMP
                    END
                    ELSE
*!!!!!!!
                        GOSUB FMT.CR.MVMT.LCY
                        GOSUB FMT.CR.MVMT
*!!!!!!
                        Y.FT.OUT.LIST := Y.CR.TYPE.OF.TXN:'*':Y.SAP.ACC.NO:'*':Y.LINE.CONT.COM:'*':Y.RE.STAT.LINE.BAL.CUR:'*':Y.CR.MVT.LCY:'*':Y.CR.MOVEMENT:'*':Y.IS.CONTINGENT:'*':Y.SAP.ACCT.COMP
                    END
*PACS00032519-E
                CASE Y.PARAM.REPORT.AL EQ 'FCY'
*!!!!!!!!
                    GOSUB FMT.CR.MVMT.LCY
                    GOSUB FMT.CR.MVMT
*!!!!!!!

                    Y.FT.OUT.LIST := Y.REC.DELIM
                    Y.FT.OUT.LIST := Y.CR.TYPE.OF.TXN:'*':Y.SAP.ACC.NO:'*':Y.LINE.CONT.COM:'*':Y.RE.STAT.LINE.BAL.CUR:'*':'*':Y.CR.MOVEMENT:'*':Y.IS.CONTINGENT:'*':Y.SAP.ACCT.COMP

            END CASE

        CASE Y.INTRF.AL.PL EQ 'PL'
            GOSUB CR.MOVEMENT.PL
    END CASE
RETURN
*-------------
CR.MOVEMENT.PL:
*-------------
    BEGIN CASE
*PACS00032519-S
        CASE Y.PARAM.REPORT.AL EQ 'LCY'
            Y.FT.OUT.LIST := Y.REC.DELIM
            IF Y.RE.STAT.LINE.BAL.CUR EQ LCCY THEN

*!!!!!!!
                GOSUB FMT.CR.MVMT.LCY
                GOSUB FMT.CR.MVMT
*!!!!!!

                Y.FT.OUT.LIST := Y.CR.TYPE.OF.TXN:'*':Y.SAP.ACC.NO:'*':Y.LINE.CONT.COM:'*':Y.RE.STAT.LINE.BAL.CUR:'*':Y.CR.MVT.LCY:'*':'0':'*':Y.IS.CONTINGENT:'*':Y.SAP.ACCT.COMP
            END
            ELSE
*!!!!!!!
                GOSUB FMT.CR.MVMT.LCY
                GOSUB FMT.CR.MVMT
*!!!!!!

                Y.FT.OUT.LIST := Y.CR.TYPE.OF.TXN:'*':Y.SAP.ACC.NO:'*':Y.LINE.CONT.COM:'*':Y.RE.STAT.LINE.BAL.CUR:'*':Y.CR.MVT.LCY:'*':Y.CR.MOVEMENT:'*':Y.IS.CONTINGENT:'*':Y.SAP.ACCT.COMP
            END
*PACS00032519-E
        CASE Y.PARAM.REPORT.AL EQ 'FCY'

*!!!!!!!
            GOSUB FMT.CR.MVMT.LCY
            GOSUB FMT.CR.MVMT
*!!!!!!
            Y.FT.OUT.LIST := Y.REC.DELIM
            Y.FT.OUT.LIST := Y.CR.TYPE.OF.TXN:'*':Y.SAP.ACC.NO:'*':Y.LINE.CONT.COM:'*':Y.RE.STAT.LINE.BAL.CUR:'*':'*':Y.CR.MOVEMENT:'*':Y.IS.CONTINGENT:'*':Y.SAP.ACCT.COMP

    END CASE
RETURN

*--------------
DB.MOVEMENT.AL:
*--------------
    BEGIN CASE

        CASE Y.INTRF.AL.PL EQ 'AL'
            BEGIN CASE
*PACS00032519-S
                CASE Y.PARAM.REPORT.AL EQ 'LCY'
                    Y.FT.OUT.LIST := Y.REC.DELIM
                    IF Y.RE.STAT.LINE.BAL.CUR EQ LCCY THEN
*!!!!!
                        GOSUB FMT.DB.MVMT
                        GOSUB FMT.DB.MVMT.LCY
*!!!!!

                        Y.FT.OUT.LIST := Y.DB.TYPE.OF.TXN:'*':Y.SAP.ACC.NO:'*':Y.LINE.CONT.COM:'*':Y.RE.STAT.LINE.BAL.CUR:'*':Y.DB.MVT.LCY:'*':'0':'*':Y.IS.CONTINGENT:'*':Y.SAP.ACCT.COMP
                    END
                    ELSE

*!!!!!!!!!!!!!
                        GOSUB FMT.DB.MVMT
                        GOSUB FMT.DB.MVMT.LCY
*!!!!!!!!!!!
                        Y.FT.OUT.LIST := Y.DB.TYPE.OF.TXN:'*':Y.SAP.ACC.NO:'*':Y.LINE.CONT.COM:'*':Y.RE.STAT.LINE.BAL.CUR:'*':Y.DB.MVT.LCY:'*':Y.DB.MOVEMENT:'*':Y.IS.CONTINGENT:'*':Y.SAP.ACCT.COMP
                    END

                CASE Y.PARAM.REPORT.AL EQ 'FCY'
                    Y.FT.OUT.LIST := Y.REC.DELIM
*!!!!!!!!
                    GOSUB FMT.DB.MVMT
                    GOSUB FMT.DB.MVMT.LCY
*!!!!!!!
                    Y.FT.OUT.LIST := Y.DB.TYPE.OF.TXN:'*':Y.SAP.ACC.NO:'*':Y.LINE.CONT.COM:'*':Y.RE.STAT.LINE.BAL.CUR:'*':'*':Y.DB.MOVEMENT:'*':Y.IS.CONTINGENT:'*':Y.SAP.ACCT.COMP

            END CASE

        CASE Y.INTRF.AL.PL EQ 'PL'
            GOSUB DB.MOVEMENT.PL
    END CASE
RETURN

*-------------
DB.MOVEMENT.PL:
*--------------
    BEGIN CASE

        CASE Y.PARAM.REPORT.AL EQ 'LCY'
            Y.FT.OUT.LIST := Y.REC.DELIM
            IF Y.RE.STAT.LINE.BAL.CUR EQ LCCY THEN

*!!!!!!!
                GOSUB FMT.DB.MVMT
                GOSUB FMT.DB.MVMT.LCY
*!!!!!!!
                Y.FT.OUT.LIST := Y.DB.TYPE.OF.TXN:'*':Y.SAP.ACC.NO:'*':Y.LINE.CONT.COM:'*':Y.RE.STAT.LINE.BAL.CUR:'*':Y.DB.MVT.LCY:'*':'0':'*':Y.IS.CONTINGENT:'*':Y.SAP.ACCT.COMP
            END
            ELSE
*!!!!!!
                GOSUB FMT.DB.MVMT
                GOSUB FMT.DB.MVMT.LCY
*!!!!!!
                Y.FT.OUT.LIST := Y.DB.TYPE.OF.TXN:'*':Y.SAP.ACC.NO:'*':Y.LINE.CONT.COM:'*':Y.RE.STAT.LINE.BAL.CUR:'*':Y.DB.MVT.LCY:'*':Y.DB.MOVEMENT:'*':Y.IS.CONTINGENT:'*':Y.SAP.ACCT.COMP
            END
*PACS00032519-E
        CASE Y.PARAM.REPORT.AL EQ 'FCY'
            Y.FT.OUT.LIST := Y.REC.DELIM
*!!!!!!!!
            GOSUB FMT.DB.MVMT
            GOSUB FMT.DB.MVMT.LCY
*!!!!!!!
            Y.FT.OUT.LIST := Y.DB.TYPE.OF.TXN:'*':Y.SAP.ACC.NO:'*':Y.LINE.CONT.COM:'*':Y.RE.STAT.LINE.BAL.CUR:'*':'*':Y.DB.MOVEMENT:'*':Y.IS.CONTINGENT:'*':Y.SAP.ACCT.COMP

    END CASE
RETURN
*-----------*
FMT.CR.MVMT:
*-----------*
    Y.CR.MVT.DEC.VAL=''
    Y.CR.MVT.INT.VAL=''
    FMT.CR.DEC.MVMT=''
    Y.CR.MVMT=0
    Y.CR.MVMT = R.RE.STAT.LINE.BAL<RE.SLB.CR.MOVEMENT> + 0
    Y.CR.MVT.DEC.VAL=FIELD(Y.CR.MVMT,'.',2)
    Y.CR.MVT.INT.VAL=FIELD(Y.CR.MVMT,'.',1)
    IF NOT(Y.CR.MVT.INT.VAL) THEN
        Y.CR.MVT.INT.VAL=''
    END
    FMT.CR.DEC.MVMT=FMT(Y.CR.MVT.DEC.VAL,'L%2')
    Y.CR.MOVEMENT=Y.CR.MVT.INT.VAL:FMT.CR.DEC.MVMT
RETURN

*---------------*
FMT.CR.MVMT.LCY:
*---------------*
    Y.CR.MVT.LCY.DEC.VAL=''
    Y.CR.MVT.LCY.INT.VAL=''
    FMT.CR.DEC.MVMT.LCY=''
    Y.CR.MVMT.LCY=0
    Y.CR.MVMT.LCY = R.RE.STAT.LINE.BAL<RE.SLB.CR.MVMT.LCL> + 0
    Y.CR.MVT.LCY.DEC.VAL=FIELD(Y.CR.MVMT.LCY,'.',2)
    Y.CR.MVT.LCY.INT.VAL=FIELD(Y.CR.MVMT.LCY,'.',1)
    IF NOT(Y.CR.MVT.LCY.INT.VAL) THEN
        Y.CR.MVT.LCY.INT.VAL=''
    END
    FMT.CR.DEC.MVMT.LCY=FMT(Y.CR.MVT.LCY.DEC.VAL,'L%2')
    Y.CR.MVT.LCY=Y.CR.MVT.LCY.INT.VAL:FMT.CR.DEC.MVMT.LCY
RETURN

*-----------*
FMT.DB.MVMT:
*-----------*

    Y.DB.MVMT=''
    Y.DB.MVT.DEC.VAL=''
    Y.DB.MVT.INT.VAL=''
    FMT.DB.DEC.MVMT=''
    Y.DB.MVMT=0
    Y.DB.MVMT = ABS(R.RE.STAT.LINE.BAL<RE.SLB.DB.MOVEMENT>) + 0
    Y.DB.MVT.DEC.VAL=FIELD(Y.DB.MVMT,'.',2)
    Y.DB.MVT.INT.VAL=FIELD(Y.DB.MVMT,'.',1)
    IF NOT(Y.DB.MVT.INT.VAL) THEN
        Y.DB.MVT.INT.VAL=''
    END
    FMT.DB.DEC.MVMT=FMT(Y.DB.MVT.DEC.VAL,'L%2')
    Y.DB.MOVEMENT=Y.DB.MVT.INT.VAL:FMT.DB.DEC.MVMT
RETURN
*---------------*
FMT.DB.MVMT.LCY:
*---------------*

    Y.DB.MVMT.LCY=''
    Y.DB.MVT.LCY.DEC.VAL=''
    Y.DB.MVT.LCY.INT.VAL=''
    FMT.DB.DEC.MVMT.LCY=''
    Y.DB.MVMT.LCY=0
    Y.DB.MVMT.LCY = ABS(R.RE.STAT.LINE.BAL<RE.SLB.DB.MVMT.LCL>)  + 0
    Y.DB.MVT.LCY.DEC.VAL=FIELD(Y.DB.MVMT.LCY,'.',2)
    Y.DB.MVT.LCY.INT.VAL=FIELD(Y.DB.MVMT.LCY,'.',1)
    IF NOT(Y.DB.MVT.LCY.INT.VAL) THEN
        Y.DB.MVT.LCY.INT.VAL=''
    END
    FMT.DB.DEC.MVMT.LCY=FMT(Y.DB.MVT.LCY.DEC.VAL,'L%2')
    Y.DB.MVT.LCY=Y.DB.MVT.LCY.INT.VAL:FMT.DB.DEC.MVMT.LCY
RETURN


END
