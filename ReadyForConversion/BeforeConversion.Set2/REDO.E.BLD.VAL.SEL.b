*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.E.BLD.VAL.SEL(ENQ.DATA)
    $INCLUDE T24.BP I_EQUATE
    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_ENQUIRY.COMMON

    IF NOT(ENQ.DATA<4>) THEN 
        ENQ.ERROR = 'EB-SEL.MAND'
        CALL STORE.END.ERROR
        GOSUB END1
    END
    RETURN
END1:
END
