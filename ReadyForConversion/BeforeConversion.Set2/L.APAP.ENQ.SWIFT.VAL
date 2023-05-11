*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* <Rating>-5</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.ENQ.SWIFT.VAL(Y.FINAL)

*--------------------------------------------------------------------------------------------------
* Description           : This routine returns a status and 
* Developed On          : 24/07/2020
* Developed By          : Estalin Valerio
* Development Reference : ---
*--------------------------------------------------------------------------------------------------
*  M O D I F I C A T I O N S
* ***************************
* Defect Reference       Modified By                    Date of Change        Change Details
* --------               Estalin Valerio                24/07/2020            Creation
*--------------------------------------------------------------------------------------------------
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_ENQUIRY.COMMON
    $INSERT T24.BP I_F.ACCOUNT
    $INSERT TAM.BP I_F.LATAM.CARD.CUSTOMER
    $INSERT T24.BP I_F.ALTERNATE.ACCOUNT

**---------------------------------------
**ABRIR LA TABLA F.ACCOUNT 
**---------------------------------------
    FN.ACC = "F.ACCOUNT"
    FV.ACC = ""
    R.ACC = ""
    ACC.ERR = ""

    FN.ALT.ACC = "F.ALTERNATE.ACCOUNT"
    FV.ALT.ACC = ""
    R.ALT.ACC = ""
    ACC.ALT.ERR = ""

    FN.CARD.CUSTOMER  = "F.LATAM.CARD.CUSTOMER"
    F.CARD.CUSTOMER  = ""
    CALL OPF(FN.CARD.CUSTOMER,F.CARD.CUSTOMER)

**-------------------------------------------------------------

**---------------------------------------------------------------------------------------------
**SENTENCIA LOCATE
**---------------------------------------------------------------------------------------------
    LOCATE "CUSTOMER.NUMBER" IN D.FIELDS<1> SETTING CUS.POS THEN
        F.ID = D.RANGE.AND.VALUE<CUS.POS>
    END

    LOCATE "CUSTOMER.ACCOUNT" IN D.FIELDS<2> SETTING ACC.POS THEN
        F.ACCOUNT = D.RANGE.AND.VALUE<ACC.POS>
    END 

  **-----------------------------------------------------------------------------------
  ** Buscar cliente de la cuenta
  **-----------------------------------------------------------------------------------

  **---------------------------VALIDAR SI ES UNA TARJETA------------------------------- 
    IF LEN(F.ACCOUNT) EQ 4 THEN 
      
        CALL F.READ(FN.CARD.CUSTOMER,F.ID,R.CARD.CUSTOMER,F.CARD.CUSTOMER,ERROR.CUSTOMER)
       
        Y.CARDS = R.CARD.CUSTOMER<APAP.DC.CARD.NO>
        Y.CUENTAS = R.CARD.CUSTOMER<APAP.DC.ACCOUNT.NO>
        Y.CARDS.COUNT = DCOUNT(Y.CARDS,@VM)
      
        FOR A=1 TO Y.CARDS.COUNT STEP 1   
        
        Y.CARDS.CU = Y.CARDS<1,A>

        IF RIGHT(Y.CARDS.CU,4) EQ F.ACCOUNT THEN
           Y.CUENTA = Y.CUENTAS<1,A>
        END

        NEXT A
    
        IF (Y.CUENTA NE "") THEN
            F.ACCOUNT = Y.CUENTA
        END

    END 

  **---------------------------BUSCAR LA CUENTA EN ALTERNATE.ACCOUNT------------------------------- 
  IF LEN(F.ACCOUNT) GT 10 AND LEFT(F.ACCOUNT,1) NE 1 THEN
  
      CALL F.READ(FN.ALT.ACC,F.ACCOUNT,R.ALT.ACC, FV.ALT.ACC, ACC.ALT.ERR)
      Y.ALT.ACCOUNT = R.ALT.ACC<AAC.GLOBUS.ACCT.NUMBER> 
    
      IF Y.ALT.ACCOUNT NE "" THEN 
         F.ACCOUNT = Y.ALT.ACCOUNT
      END 
  END

  CALL F.READ(FN.ACC,F.ACCOUNT,R.ACC, FV.ACC, ACC.ERR)
  **--------------------Estado de la cuenta----------------------**
  Y.RECORD.STATUS  = R.ACC<AC.RECORD.STATUS> 

   CALL GET.LOC.REF("ACCOUNT", "L.AC.STATUS1", L.AC.STATUS1.POS)
   Y.L.AC.STATUS1  = R.ACC<AC.LOCAL.REF,L.AC.STATUS1.POS>
  
  ** --------- Validar Cliente-------**
  Y.CUSTOMER        = R.ACC<AC.CUSTOMER> 
  Y.ACCOUNT.NUMBER  = F.ACCOUNT
  Y.JOINT.PRIMERO   = R.ACC<AC.JOINT.HOLDER,1>
  Y.JOINT.SEGUNDO   = R.ACC<AC.JOINT.HOLDER,2>
  
   Y.ACCOUNT.VALID = 0 
   IF (Y.CUSTOMER NE "" AND F.ID NE "") THEN 
    IF Y.CUSTOMER EQ F.ID OR F.ID EQ Y.JOINT.PRIMERO OR  F.ID EQ Y.JOINT.SEGUNDO THEN
            Y.ACCOUNT.VALID = 1        
    END 
   END 

   IF Y.ACCOUNT.VALID EQ 0 THEN
       Y.ACCOUNT.VALID = "FALSE"
   END 
   ELSE 
       Y.ACCOUNT.VALID = "TRUE"
   END 

    Y.FINAL<-1> = Y.ACCOUNT.VALID : "*" : Y.L.AC.STATUS1 
    
   RETURN

END
