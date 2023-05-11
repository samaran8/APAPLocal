*------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* <Rating>95</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE  LAPAP.AVOID.GARNISHMENT
*------------------------------------------------------------------------------------
* Technical report:
* -----------------
* Company Name   : APAP
* Program Name   : LAPAP.AVOID.GARNISHMENT
* Date           : 2018-02-16
* Item ID        : CN008230
*------------------------------------------------------------------------------------
* Description :
* ------------
* This routine avoid garnishment when according to the law client has less than X amount
*------------------------------------------------------------------------------------
* Modification History :
* ----------------------
* Date           Author            Modification Description
* -------------  -----------       ---------------------------
* 2018-01-16     Richard HC        Initial Development
*------------------------------------------------------------------------------------
* Content summary :
* -----------------
* Table name     : N/A
* Auto Increment : N/A
* Views/versions : N/A
* EB record      : LAPAP.AVOID.GARNISHMENT
* Routine        : LAPAP.AVOID.GARNISHMENT
* Overrride      : AVOID.GARNISHMENT.OVERRIDE
*------------------------------------------------------------------------------------


    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_F.ACCOUNT
    $INSERT TAM.BP I_F.REDO.CUST.PRD.LIST
    $INSERT TAM.BP I_F.APAP.H.GARNISH.DETAILS 

    FN.CUS.PRD = "F.REDO.CUST.PRD.LIST"
    F.CUS.PRD = ""
    FN.ACCOUNT = "F.ACCOUNT"
    F.ACCOUNT = ""

    CALL OPF(FN.CUS.PRD, F.CUS.PRD)
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    ERR.CUS.PRD = "";
    ERR.ACCOUNT = "";
    R.CUS.PRD = "";
    R.ACCOUNT = "";

     *CUS.PRD.ID = 1000198      ;*1000401;
     CUS.PRD.ID = R.NEW(APAP.GAR.CUSTOMER)
     *CUSTOMER.ID = R.NEW(APAP.GAR.CUSTOMER)

       IF CUS.PRD.ID ! '' THEN

        CALL F.READ(FN.CUS.PRD,CUS.PRD.ID,R.CUS.PRD,F.CUS.PRD,ERR.CUS.PRD);

        PRD.ID = R.CUS.PRD<PRD.PRODUCT.ID>

        VAL = 0
        M = DCOUNT(PRD.ID,@VM)
        FOR A = 1 TO M STEP 1

            PRD.ID = R.CUS.PRD<1,A>

            CALL F.READ(FN.ACCOUNT,PRD.ID,R.ACCOUNT,F.ACCOUNT,ERR.ACCOUNT);
            CALL GET.LOC.REF("ACCOUNT","L.AC.AV.BAL",POS)
            VAL = R.ACCOUNT<AC.LOCAL.REF,POS>
            REL.CODE = R.ACCOUNT<AC.RELATION.CODE>


            IF REL.CODE EQ '500' OR REL.CODE EQ '501' OR REL.CODE EQ '' THEN

            * AC.LOCAL.REF
            *Assignin valor to VAL variable
            *VAL = R.ACCOUNT<AC.OPEN.CLEARED.BAL>
            CTG = R.ACCOUNT<AC.CATEGORY>

            IF (CTG GT '6600' AND CTG LT '6620') OR (CTG GT '6000' AND CTG LT '6599') THEN

                *Adding Element to array
                V.ARRAY<-1> = VAL 

            END
        NEXT A

        GARNISH.AMOUNT = SUM(V.ARRAY)

        PRINT @(40,8) : GARNISH.AMOUNT
        IF GARNISH.AMOUNT LT 1000 THEN

            CALL LAPAP.AVOID.GARNISHMENT.FLD

            TEXT = "AVOID.GARNISHMENT.OVERRIDE"
            CURR.NO = 1
            CALL STORE.OVERRIDE(CURR.NO)
            RETURN

        END


    END

END
