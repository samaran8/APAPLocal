*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.CAMPANA.SUMULATION
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.AA.CUSTOMER
    $INSERT TAM.BP I_F.REDO.CAMPAIGN.TYPES
    GOSUB GET.CAMPANA.CUSTOMER
    GOSUB GET.TIPO.CAMPANA
    RETURN

GET.CAMPANA.CUSTOMER:
********************
    AA.ID = COMI
    CALL REDO.B.CON.LNS.BY.DEBTOR.AA.RECS(AA.ID,OUT.RECORD)
    R.AA.CUSTOMER             = FIELD(OUT.RECORD,"*",9)
    RETURN
GET.TIPO.CAMPANA:
*****************
    FN.REDO.CAMPAIGN.TYPES = 'F.REDO.CAMPAIGN.TYPES'; F.REDO.CAMPAIGN.TYPES = ''
    CALL OPF(FN.REDO.CAMPAIGN.TYPES,F.REDO.CAMPAIGN.TYPES)
    Y.APPL = "AA.PRD.DES.CUSTOMER"
    Y.FLD  = "L.AA.CAMP.TY"
    Y.POS  = ""
    CALL MULTI.GET.LOC.REF(Y.APPL,Y.FLD,Y.POS)
    L.AA.CAMP.TY.POS = Y.POS<1,1>
    YL.AA.CAMP.TY = R.AA.CUSTOMER<AA.CUS.LOCAL.REF,L.AA.CAMP.TY.POS>
    ERR.REDO.CAMPAIGN.TYPES = ''; R.REDO.CAMPAIGN.TYPES = '';
    CALL F.READ(FN.REDO.CAMPAIGN.TYPES,YL.AA.CAMP.TY,R.REDO.CAMPAIGN.TYPES,F.REDO.CAMPAIGN.TYPES,ERR.REDO.CAMPAIGN.TYPES)
    IF R.REDO.CAMPAIGN.TYPES THEN
        Y.CAM.TYPE.DESC = R.REDO.CAMPAIGN.TYPES<CG.TYP.CAM.TYPE.DESC>
    END
    COMI = Y.CAM.TYPE.DESC
    RETURN
END