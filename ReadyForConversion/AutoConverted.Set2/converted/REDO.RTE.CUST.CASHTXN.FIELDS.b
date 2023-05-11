SUBROUTINE REDO.RTE.CUST.CASHTXN.FIELDS

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
    $INSERT I_Table

    ID.F='@ID'
    ID.N='50'
    ID.T='A'

    CALL Table.addFieldDefinition("XX<TXN.ID", 16, 'A', "") ;* Add a new field
    CALL Table.addFieldDefinition("XX-INITIAL.ID", 16, 'A', "")       ;* Add a new field
    CALL Table.addFieldDefinition("XX-TRANS.DATE", 25, 'A', "")       ;* Add a new field
    CALL Table.addField("XX-BRANCH.CODE", 'A', "", "")      ;* Add a new fields
    CALL Field.setCheckFile("COMPANY")  ;* Use DEFAULT.ENRICH from SS or just field 1

    CALL Table.addFieldDefinition("XX-CASH.AMOUNT", 19, 'AMT', "")    ;* Add a new field

    CALL Table.addFieldDefinition("XX-ACTUAL.VERSION", 65, 'A', "")   ;* Add a new field
    CALL Table.addFieldDefinition("XX-FUNCTION", 3, 'A', "")          ;* Add a new field
    CALL Table.addFieldDefinition("XX>RTE.RESET.FLAG", 10, 'A', "")          ;* Add a new field

    V = Table.currentFieldPosition

RETURN

END
