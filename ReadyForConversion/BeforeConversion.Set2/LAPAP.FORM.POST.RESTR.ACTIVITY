*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
*Routine to form Posting Restriction Activities String using the Arrangement ID from SL.CLEAR.AA savedlist file.
*OFS string will be saved in STRING.IDS file under &SAVEDLISTS&.
*Creator: Durga Venkatraman
*-----------------------------------------------------------------------------

    SUBROUTINE LAPAP.FORM.POST.RESTR.ACTIVITY(AA.ID)
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.AA.ARRANGEMENT

    GOSUB OPEN.FILES
    GOSUB FORM.OFS.STR

    RETURN

**************
OPEN.FILES:
**************
    Y.AA.ID = AA.ID
    FN.AA = 'F.AA.ARRANGEMENT'
    F.AA = ''
    CALL OPF(FN.AA,F.AA)

    FN.SL = './&SAVEDLISTS&'
    F.SL = ''
    CALL OPF(FN.SL,F.SL)
    STRING.ID = ''

    RETURN
**************
FORM.OFS.STR:
**************
    R.AA = ''
    READ R.AA FROM F.AA,Y.AA.ID THEN
        Y.CO.CODE = ''; Y.CO.CODE = R.AA<AA.ARR.CO.CODE>
        Y.TODAY = ''; Y.TODAY = TODAY
        STRING.ID<-1> = "AA.ARRANGEMENT.ACTIVITY,TEST/I/PROCESS//0,//":Y.CO.CODE:",,ARRANGEMENT:1:1=":Y.AA.ID:",ACTIVITY:1:1=LENDING-UPDATE-POST.RESTRICT,EFFECTIVE.DATE:1:1=":Y.TODAY:",PROPERTY:1:1=ACCOUNT,FIELD.NAME:1=POSTING.RESTRICT,FIELD.VALUE:1=75"
        STRING.ID<-1> = "AA.ARRANGEMENT.ACTIVITY,TEST/I/PROCESS//0,//":Y.CO.CODE:",,ARRANGEMENT:1:1=":Y.AA.ID:",ACTIVITY:1:1=LENDING-UPDATE-OD.STATUS,EFFECTIVE.DATE:1:1=":Y.TODAY:",PROPERTY:1:1=ACCOUNT,FIELD.NAME:1:1=L.OD.STATUS:1:1,FIELD.VALUE:1:1=CUR,FIELD.NAME:1:2=L.OD.STATUS.2:1:1,FIELD.VALUE:1:2=CUR"
    END
    WRITE STRING.ID TO F.SL,'STRING.IDS'
    Y.LIST.NAME = 'STRING.IDS'
    Y.LIST.NAME.EB = 'AA.ADJ.BAL'
    CALL LAPAP.TRG.OFS.STRING(Y.LIST.NAME)
    CALL LAPAP.PACS.RAISE.ENTRY(Y.LIST.NAME.EB)
    RETURN
