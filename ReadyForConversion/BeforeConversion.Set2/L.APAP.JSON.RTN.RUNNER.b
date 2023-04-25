*-----------------------------------------------------------------------------
* <Rating>-2</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.JSON.RTN.RUNNER(Y.OBJECT.NAME, JSON.REQUEST, Y.DYN.RESPONSE.KEY, Y.DYN.RESPONSE.VALUE, Y.DYN.RESPONSE.TYPE, Y.ERROR)
    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE

*Subroutine RUNNER
*----------------------------------------------------------------------------------------------------------------------------------------------------
*CLEAR OUTPUT VARIABLES
Y.DYN.RESPONSE.KEY = ''
Y.DYN.RESPONSE.VALUE = ''
Y.DYN.RESPONSE.TYPE = ''
Y.ERROR = ''
Y.ERROR<3> = 'L.APAP.JSON.RTN.RUNNER'

    BEGIN CASE
    CASE Y.OBJECT.NAME EQ 'ANY.ROUTINE'
        **CALL ANY.ROUTINE(JSON.REQUEST, Y.DYN.RESPONSE.KEY, Y.DYN.RESPONSE.VALUE, Y.DYN.RESPONSE.TYPE, Y.ERROR)
    CASE 1
        Y.ERROR<1> = '1'
        Y.ERROR<2> = 'INVALID ROUTINTE'
    END CASE
    RETURN
END
