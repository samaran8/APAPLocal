*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.FR.CL.AUT.CC.RT
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.REDO.FRONT.CLAIMS
    $INSERT I_F.REDO.ISSUE.CLAIMS


    FN.CUS = "F.CUSTOMER"
    FV.CUS = ""
    R.CUS = ""
    CUS.ERR = ""
    CALL OPF(FN.CUS,FV.CUS)
    SEL.CMD = ""
    SEL.LIST = ""
    NO.OF.RECS = ""
    SEL.ERR = ""

    Y.CUST.CODE.IN = R.NEW(FR.CL.CUSTOMER.CODE)
    Y.CUST.CODE.OUT = Y.CUST.CODE.IN
    CALL F.READ(FN.CUS,Y.CUST.CODE.IN,R.CUS, FV.CUS, CUS.ERR)
    IF R.CUS NE '' THEN
        Y.CUST.CODE.OUT = Y.CUST.CODE.IN
    END
    IF R.CUS EQ '' THEN
        SEL.CMD = "SELECT " : FN.CUS : " WITH L.CU.CIDENT EQ " : Y.CUST.CODE.IN
        CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.RECS,SEL.ERR)
        LOOP REMOVE Y.CUSTOMER.CODE FROM SEL.LIST SETTING STMT.POS

        WHILE Y.CUSTOMER.CODE DO
            CALL F.READ(FN.CUS,Y.CUSTOMER.CODE,R.CUS, FV.CUS, CUS.ERR)
            IF R.CUS NE '' THEN
                Y.CUST.CODE.OUT = Y.CUSTOMER.CODE
            END
        REPEAT
    END
    R.NEW(ISS.CL.CUSTOMER.CODE) = Y.CUST.CODE.OUT
    RETURN
END
