parser grammar VQFlowParser;

options {
    tokenVocab = VQFlowLexer;
}

target:
    query EOF?
    | selectStatement (SEMICOLON_SYMBOL EOF? | EOF)
    | EOF
;
query:
   TARGET_QUERY OPEN_PAR_SYMBOL
       description?
       queryBody
   CLOSE_PAR_SYMBOL
;

requirementsModule:
    identifier equal REQUIREMENT_MODULE OPEN_PAR_SYMBOL
        description?
        queryBody
    CLOSE_PAR_SYMBOL
;

miniQuery:
    identifier equal MINI_QUERY OPEN_PAR_SYMBOL
        singleTableRef 
        filterExpr?
        groupByClause?
        havingClause?
        selectClause
        orderClause?
        limitClause?
    CLOSE_PAR_SYMBOL
;

singleTableRef:
    tableRef
;
selectClause:
    selectItemList
;
filterExpr:
    FILTER_SYMBOL expr
;

queryBody:
    table+ | expandQuery | unionQuery
;

table:
    requirementsModule | miniQuery
;

expandQuery:
    table table (table | expandEdge)*
;

expandEdge:
    identifier JSON_SEPARATOR_SYMBOL OPEN_PAR_SYMBOL expandEdgeBody CLOSE_PAR_SYMBOL JSON_SEPARATOR_SYMBOL identifier
;
expandEdgeBody:
    (cardinality COMMA_SYMBOL)? columnsToMatch?
;

cardinality:
   ONE | CARDINALITYN
;

columnsToMatch:
    columnToMatch (COMMA_SYMBOL columnToMatch )*
;

columnToMatch:
    identifier equal identifier
;

unionQuery:
    table table+ unionEdge
;

unionEdge:
    UNION_SYMBOL unionOption identifier identifier+
;

description:
    DOUBLE_QUOTED_TEXT
;

selectStatement:
    queryExpression
    | queryExpressionParens
;


queryExpressionParens:
    OPEN_PAR_SYMBOL (
        queryExpressionParens
        | queryExpression
    ) CLOSE_PAR_SYMBOL
;
queryExpressionBody:
    (
        queryPrimary
        | queryExpressionParens UNION_SYMBOL unionOption? (
            queryPrimary
            | queryExpressionParens
        )
    ) (UNION_SYMBOL unionOption? ( queryPrimary | queryExpressionParens))*
;
queryExpression:
    ( withClause)? (
        queryExpressionBody orderClause? limitClause?
        | queryExpressionParens orderClause? limitClause?
    )
;
selectItemList: (selectItem | MULT_OPERATOR) (COMMA_SYMBOL selectItem)*
;

selectItem:
    tableWild
    | expr selectAlias?
;

expr:
    boolPri (IS_SYMBOL notRule? type = (TRUE_SYMBOL | FALSE_SYMBOL | UNKNOWN_SYMBOL))? # exprIs
    | NOT_SYMBOL expr                                                                  # exprNot
    | expr op = (AND_SYMBOL | LOGICAL_AND_OPERATOR) expr                               # exprAnd
    | expr XOR_SYMBOL expr                                                             # exprXor
    | expr op = (OR_SYMBOL | LOGICAL_OR_OPERATOR) expr                                 # exprOr
;

boolPri:
    predicate                                           # primaryExprPredicate
    | boolPri IS_SYMBOL notRule? NULL_SYMBOL            # primaryExprIsNull
    | boolPri compOp predicate                          # primaryExprCompare
    | boolPri compOp (ALL_SYMBOL | ANY_SYMBOL) subquery # primaryExprAllAny
;
compOp:
    EQUAL_OPERATOR
    | NULL_SAFE_EQUAL_OPERATOR
    | GREATER_OR_EQUAL_OPERATOR
    | GREATER_THAN_OPERATOR
    | LESS_OR_EQUAL_OPERATOR
    | LESS_THAN_OPERATOR
    | NOT_EQUAL_OPERATOR
;

subquery:
    queryExpressionParens
;

withClause:
    WITH_SYMBOL RECURSIVE_SYMBOL? commonTableExpression (
        COMMA_SYMBOL commonTableExpression
    )*
;
commonTableExpression:
    identifier columnInternalRefList? AS_SYMBOL subquery
;
limitClause:
    LIMIT_SYMBOL limitOptions
;
limitOptions:
    limitOption ((COMMA_SYMBOL | OFFSET_SYMBOL) limitOption)?
;
limitOption:
    identifier
    | (PARAM_MARKER | ULONGLONG_NUMBER | LONG_NUMBER | INT_NUMBER)
;

queryPrimary:
    querySpecification
;
tableRef:
    qualifiedIdentifier
    | dotIdentifier
;
querySpecification:
    SELECT_SYMBOL selectItemList fromClause? whereClause? groupByClause? havingClause?
;
groupByClause:
    GROUP_SYMBOL BY_SYMBOL orderList olapOption?
;
olapOption:
    WITH_SYMBOL ROLLUP_SYMBOL
    |  WITH_SYMBOL CUBE_SYMBOL
;
havingClause:
    HAVING_SYMBOL expr
;
whereClause:
    WHERE_SYMBOL expr
;
fromClause:
    FROM_SYMBOL (DUAL_SYMBOL | tableReferenceList)
;
tableReferenceList:
    tableReference (COMMA_SYMBOL tableReference)*
;
tableReference: ( // Note: we have also a tableRef rule for identifiers that reference a table anywhere.
        tableFactor
        | OPEN_CURLY_SYMBOL ( identifier | OJ_SYMBOL) escapedTableReference CLOSE_CURLY_SYMBOL // ODBC syntax
    ) joinedTable*
;
escapedTableReference:
    tableFactor joinedTable*
;
joinedTable: // Same as joined_table in sql_yacc.yy, but with removed left recursion.
    innerJoinType tableReference (
        ON_SYMBOL expr
        | USING_SYMBOL identifierListWithParentheses
    )?
    | outerJoinType tableReference (
        ON_SYMBOL expr
        | USING_SYMBOL identifierListWithParentheses
    )
    | naturalJoinType tableFactor
;
naturalJoinType:
    NATURAL_SYMBOL INNER_SYMBOL? JOIN_SYMBOL
    | NATURAL_SYMBOL (LEFT_SYMBOL | RIGHT_SYMBOL) OUTER_SYMBOL? JOIN_SYMBOL
;
innerJoinType:
    type = (INNER_SYMBOL | CROSS_SYMBOL)? JOIN_SYMBOL
    | type = STRAIGHT_JOIN_SYMBOL
;
outerJoinType:
    type = (LEFT_SYMBOL | RIGHT_SYMBOL) OUTER_SYMBOL? JOIN_SYMBOL
;
tableFactor:
    singleTable
    | singleTableParens
    | derivedTable
    | tableReferenceListParens
;
singleTable:
    tableRef tableAlias?
;

singleTableParens:
    OPEN_PAR_SYMBOL (singleTable | singleTableParens) CLOSE_PAR_SYMBOL
;

identifierListWithParentheses:
    OPEN_PAR_SYMBOL identifierList CLOSE_PAR_SYMBOL
;
identifierList: // ident_string_list in sql_yacc.yy.
    identifier (COMMA_SYMBOL identifier)*
;

derivedTable:
    subquery tableAlias? ( columnInternalRefList)?
    |  LATERAL_SYMBOL subquery tableAlias? columnInternalRefList?
;
columnInternalRefList: // column_list (+ parentheses) + opt_derived_column_list in sql_yacc.yy
    OPEN_PAR_SYMBOL columnInternalRef (COMMA_SYMBOL columnInternalRef)* CLOSE_PAR_SYMBOL
;
columnInternalRef:
    identifier
;
tableAlias:
    (AS_SYMBOL |  EQUAL_OPERATOR)? identifier
;

// This rule covers both: joined_table_parens and table_reference_list_parens from sql_yacc.yy.
// We can simplify that because we have unrolled the indirect left recursion in joined_table <-> table_reference.
tableReferenceListParens:
    OPEN_PAR_SYMBOL (tableReferenceList | tableReferenceListParens) CLOSE_PAR_SYMBOL
;

unionOption:
    DISTINCT_SYMBOL
    | ALL_SYMBOL
;
notRule:
    NOT_SYMBOL
    | NOT2_SYMBOL // A NOT with a different (higher) operator precedence.
;

predicate:
    bitExpr (
        notRule? predicateOperations
        |  MEMBER_SYMBOL OF_SYMBOL? simpleExprWithParentheses
        | SOUNDS_SYMBOL LIKE_SYMBOL bitExpr
    )?
;
predicateOperations:
    IN_SYMBOL (subquery | OPEN_PAR_SYMBOL exprList CLOSE_PAR_SYMBOL) # predicateExprIn
    | BETWEEN_SYMBOL bitExpr AND_SYMBOL predicate                    # predicateExprBetween
    | LIKE_SYMBOL simpleExpr (ESCAPE_SYMBOL simpleExpr)?             # predicateExprLike
    | REGEXP_SYMBOL bitExpr                                          # predicateExprRegex
;
columnRef: // A field identifier that can reference any schema/table.
    fieldIdentifier
;
fieldIdentifier:
    dotIdentifier
    | qualifiedIdentifier dotIdentifier?
;
simpleExpr:
    variable (equal expr)?                                                                               # simpleExprVariable
    | columnRef                                                                            # simpleExprColumnRef
    | runtimeFunctionCall                                                                                # simpleExprRuntimeFunction
    | functionCall                                                                                       # simpleExprFunction
    | simpleExpr COLLATE_SYMBOL textOrIdentifier                                                         # simpleExprCollate
    | literal                                                                                            # simpleExprLiteral
    | PARAM_MARKER                                                                                       # simpleExprParamMarker
    | sumExpr                                                                                            # simpleExprSum
    |  groupingOperation                                                        # simpleExprGroupingOperation
    | simpleExpr CONCAT_PIPES_SYMBOL simpleExpr                                                          # simpleExprConcat
    | op = (PLUS_OPERATOR | MINUS_OPERATOR | BITWISE_NOT_OPERATOR) simpleExpr                            # simpleExprUnary
    | not2Rule simpleExpr                                                                                # simpleExprNot
    | ROW_SYMBOL? OPEN_PAR_SYMBOL exprList CLOSE_PAR_SYMBOL                                              # simpleExprList
    | EXISTS_SYMBOL? subquery                                                                            # simpleExprSubQuery
    | OPEN_CURLY_SYMBOL identifier expr CLOSE_CURLY_SYMBOL                                               # simpleExprOdbc
    | MATCH_SYMBOL identListArg AGAINST_SYMBOL OPEN_PAR_SYMBOL bitExpr fulltextOptions? CLOSE_PAR_SYMBOL # simpleExprMatch
    | BINARY_SYMBOL simpleExpr                                                                           # simpleExprBinary
    | CAST_SYMBOL OPEN_PAR_SYMBOL expr AS_SYMBOL castType arrayCast? CLOSE_PAR_SYMBOL                    # simpleExprCast
    | CASE_SYMBOL expr? (whenExpression thenExpression)+ elseExpression? END_SYMBOL                      # simpleExprCase
    | CONVERT_SYMBOL OPEN_PAR_SYMBOL expr COMMA_SYMBOL castType CLOSE_PAR_SYMBOL                         # simpleExprConvert
    | CONVERT_SYMBOL OPEN_PAR_SYMBOL expr USING_SYMBOL charsetName CLOSE_PAR_SYMBOL                      # simpleExprConvertUsing
    | DEFAULT_SYMBOL OPEN_PAR_SYMBOL simpleIdentifier CLOSE_PAR_SYMBOL                                   # simpleExprDefault
    | VALUES_SYMBOL OPEN_PAR_SYMBOL simpleIdentifier CLOSE_PAR_SYMBOL                                    # simpleExprValues
    | INTERVAL_SYMBOL expr interval PLUS_OPERATOR expr                                                   # simpleExprInterval
;
runtimeFunctionCall:
    // Function names that are keywords.
    name = CHAR_SYMBOL OPEN_PAR_SYMBOL exprList (USING_SYMBOL charsetName)? CLOSE_PAR_SYMBOL
    | name = CURRENT_USER_SYMBOL parentheses?
    | name = DATE_SYMBOL exprWithParentheses
    | name = DAY_SYMBOL exprWithParentheses
    | name = HOUR_SYMBOL exprWithParentheses
    | name = INSERT_SYMBOL OPEN_PAR_SYMBOL expr COMMA_SYMBOL expr COMMA_SYMBOL expr COMMA_SYMBOL expr CLOSE_PAR_SYMBOL
    | name = INTERVAL_SYMBOL OPEN_PAR_SYMBOL expr (COMMA_SYMBOL expr)+ CLOSE_PAR_SYMBOL
    | name = LEFT_SYMBOL OPEN_PAR_SYMBOL expr COMMA_SYMBOL expr CLOSE_PAR_SYMBOL
    | name = MINUTE_SYMBOL exprWithParentheses
    | name = MONTH_SYMBOL exprWithParentheses
    | name = RIGHT_SYMBOL OPEN_PAR_SYMBOL expr COMMA_SYMBOL expr CLOSE_PAR_SYMBOL
    | name = SECOND_SYMBOL exprWithParentheses
    | name = TIME_SYMBOL exprWithParentheses
    | name = TIMESTAMP_SYMBOL OPEN_PAR_SYMBOL expr (COMMA_SYMBOL expr)? CLOSE_PAR_SYMBOL
    | trimFunction
    | name = USER_SYMBOL parentheses
    | name = VALUES_SYMBOL exprWithParentheses
    | name = YEAR_SYMBOL exprWithParentheses

    // Function names that are not keywords.
    | name = (ADDDATE_SYMBOL | SUBDATE_SYMBOL) OPEN_PAR_SYMBOL expr COMMA_SYMBOL (
        expr
        | INTERVAL_SYMBOL expr interval
    ) CLOSE_PAR_SYMBOL
    | name = CURDATE_SYMBOL parentheses?
    | name = CURTIME_SYMBOL timeFunctionParameters?
    | name = (DATE_ADD_SYMBOL | DATE_SUB_SYMBOL) OPEN_PAR_SYMBOL expr COMMA_SYMBOL INTERVAL_SYMBOL expr interval CLOSE_PAR_SYMBOL
    | name = EXTRACT_SYMBOL OPEN_PAR_SYMBOL interval FROM_SYMBOL expr CLOSE_PAR_SYMBOL
    | name = GET_FORMAT_SYMBOL OPEN_PAR_SYMBOL dateTimeTtype COMMA_SYMBOL expr CLOSE_PAR_SYMBOL
    | name = NOW_SYMBOL timeFunctionParameters?
    | name = POSITION_SYMBOL OPEN_PAR_SYMBOL bitExpr IN_SYMBOL expr CLOSE_PAR_SYMBOL
    | substringFunction
    | name = SYSDATE_SYMBOL timeFunctionParameters?
    | name = (TIMESTAMP_ADD_SYMBOL | TIMESTAMP_DIFF_SYMBOL) OPEN_PAR_SYMBOL intervalTimeStamp COMMA_SYMBOL expr COMMA_SYMBOL expr
        CLOSE_PAR_SYMBOL
    | name = UTC_DATE_SYMBOL parentheses?
    | name = UTC_TIME_SYMBOL timeFunctionParameters?
    | name = UTC_TIMESTAMP_SYMBOL timeFunctionParameters?

    // Function calls with other conflicts.
    | name = ASCII_SYMBOL exprWithParentheses
    | name = CHARSET_SYMBOL exprWithParentheses
    | name = COALESCE_SYMBOL exprListWithParentheses
    | name = COLLATION_SYMBOL exprWithParentheses
    | name = DATABASE_SYMBOL parentheses
    | name = IF_SYMBOL OPEN_PAR_SYMBOL expr COMMA_SYMBOL expr COMMA_SYMBOL expr CLOSE_PAR_SYMBOL
    | name = FORMAT_SYMBOL OPEN_PAR_SYMBOL expr COMMA_SYMBOL expr (COMMA_SYMBOL expr)? CLOSE_PAR_SYMBOL
    | name = MICROSECOND_SYMBOL exprWithParentheses
    | name = MOD_SYMBOL OPEN_PAR_SYMBOL expr COMMA_SYMBOL expr CLOSE_PAR_SYMBOL
    |  name = OLD_PASSWORD_SYMBOL OPEN_PAR_SYMBOL textLiteral CLOSE_PAR_SYMBOL
    |  name = PASSWORD_SYMBOL exprWithParentheses
    | name = QUARTER_SYMBOL exprWithParentheses
    | name = REPEAT_SYMBOL OPEN_PAR_SYMBOL expr COMMA_SYMBOL expr CLOSE_PAR_SYMBOL
    | name = REPLACE_SYMBOL OPEN_PAR_SYMBOL expr COMMA_SYMBOL expr COMMA_SYMBOL expr CLOSE_PAR_SYMBOL
    | name = REVERSE_SYMBOL exprWithParentheses
    | name = ROW_COUNT_SYMBOL parentheses
    | name = TRUNCATE_SYMBOL OPEN_PAR_SYMBOL expr COMMA_SYMBOL expr CLOSE_PAR_SYMBOL
    | name = WEEK_SYMBOL OPEN_PAR_SYMBOL expr (COMMA_SYMBOL expr)? CLOSE_PAR_SYMBOL
    | name = WEIGHT_STRING_SYMBOL OPEN_PAR_SYMBOL expr (
        (AS_SYMBOL CHAR_SYMBOL wsNumCodepoints)? 
        | AS_SYMBOL BINARY_SYMBOL wsNumCodepoints
        | COMMA_SYMBOL ulong_number COMMA_SYMBOL ulong_number COMMA_SYMBOL ulong_number
    ) CLOSE_PAR_SYMBOL
;
dateTimeTtype:
    DATE_SYMBOL
    | TIME_SYMBOL
    | DATETIME_SYMBOL
    | TIMESTAMP_SYMBOL
;
trimFunction:
    TRIM_SYMBOL OPEN_PAR_SYMBOL (
        expr (FROM_SYMBOL expr)?
        | LEADING_SYMBOL expr? FROM_SYMBOL expr
        | TRAILING_SYMBOL expr? FROM_SYMBOL expr
        | BOTH_SYMBOL expr? FROM_SYMBOL expr
    ) CLOSE_PAR_SYMBOL
;
timeFunctionParameters:
    OPEN_PAR_SYMBOL fractionalPrecision? CLOSE_PAR_SYMBOL
;
substringFunction:
    SUBSTRING_SYMBOL OPEN_PAR_SYMBOL expr (
        COMMA_SYMBOL expr (COMMA_SYMBOL expr)?
        | FROM_SYMBOL expr (FOR_SYMBOL expr)?
    ) CLOSE_PAR_SYMBOL
;

fractionalPrecision:
     INT_NUMBER
;
real_ulong_number:
    INT_NUMBER
    | HEX_NUMBER
    | LONG_NUMBER
    | ULONGLONG_NUMBER
;
wsNumCodepoints:
    OPEN_PAR_SYMBOL real_ulong_number CLOSE_PAR_SYMBOL
;
ulong_number:
    INT_NUMBER
    | HEX_NUMBER
    | LONG_NUMBER
    | ULONGLONG_NUMBER
    | DECIMAL_NUMBER
    | FLOAT_NUMBER
;

exprListWithParentheses:
    OPEN_PAR_SYMBOL exprList CLOSE_PAR_SYMBOL
;
equal:
    EQUAL_OPERATOR
    | ASSIGN_OPERATOR
;
qualifiedIdentifier:
    identifier dotIdentifier?
;
udfExprList:
    udfExpr (COMMA_SYMBOL udfExpr)*
;
udfExpr:
    expr selectAlias?
;
selectAlias:
    AS_SYMBOL? (identifier | textStringLiteral)
;

functionCall:
    pureIdentifier OPEN_PAR_SYMBOL udfExprList? CLOSE_PAR_SYMBOL     // For both UDF + other functions.
    | qualifiedIdentifier OPEN_PAR_SYMBOL exprList? CLOSE_PAR_SYMBOL // Other functions only.
;
fulltextOptions:
    IN_SYMBOL BOOLEAN_SYMBOL MODE_SYMBOL
    | IN_SYMBOL NATURAL_SYMBOL LANGUAGE_SYMBOL MODE_SYMBOL (
        WITH_SYMBOL QUERY_SYMBOL EXPANSION_SYMBOL
    )?
    | WITH_SYMBOL QUERY_SYMBOL EXPANSION_SYMBOL
;
literal:
    textLiteral
    | numLiteral
    | temporalLiteral
    | nullLiteral
    | boolLiteral
    | UNDERSCORE_CHARSET? (HEX_NUMBER | BIN_NUMBER)
;
numLiteral:
    INT_NUMBER
    | LONG_NUMBER
    | ULONGLONG_NUMBER
    | DECIMAL_NUMBER
    | FLOAT_NUMBER
;

boolLiteral:
    TRUE_SYMBOL
    | FALSE_SYMBOL
;

nullLiteral: // In sql_yacc.cc both 'NULL' and '\N' are mapped to NULL_SYM (which is our nullLiteral).
    NULL_SYMBOL
    | NULL2_SYMBOL
;

temporalLiteral:
    DATE_SYMBOL SINGLE_QUOTED_TEXT
    | TIME_SYMBOL SINGLE_QUOTED_TEXT
    | TIMESTAMP_SYMBOL SINGLE_QUOTED_TEXT
;
textLiteral:
    (UNDERSCORE_CHARSET? textStringLiteral | NCHAR_TEXT) textStringLiteral*
;
sumExpr:
    name = AVG_SYMBOL OPEN_PAR_SYMBOL DISTINCT_SYMBOL? inSumExpr CLOSE_PAR_SYMBOL 
    | name = (BIT_AND_SYMBOL | BIT_OR_SYMBOL | BIT_XOR_SYMBOL) OPEN_PAR_SYMBOL inSumExpr CLOSE_PAR_SYMBOL 
    | name = COUNT_SYMBOL OPEN_PAR_SYMBOL ALL_SYMBOL? MULT_OPERATOR CLOSE_PAR_SYMBOL 
    | name = COUNT_SYMBOL OPEN_PAR_SYMBOL (
        ALL_SYMBOL? MULT_OPERATOR
        | inSumExpr
        | DISTINCT_SYMBOL exprList
    ) CLOSE_PAR_SYMBOL
    | name = MIN_SYMBOL OPEN_PAR_SYMBOL DISTINCT_SYMBOL? inSumExpr CLOSE_PAR_SYMBOL 
    | name = MAX_SYMBOL OPEN_PAR_SYMBOL DISTINCT_SYMBOL? inSumExpr CLOSE_PAR_SYMBOL 
    | name = STD_SYMBOL OPEN_PAR_SYMBOL inSumExpr CLOSE_PAR_SYMBOL 
    | name = VARIANCE_SYMBOL OPEN_PAR_SYMBOL inSumExpr CLOSE_PAR_SYMBOL 
    | name = STDDEV_SAMP_SYMBOL OPEN_PAR_SYMBOL inSumExpr CLOSE_PAR_SYMBOL 
    | name = VAR_SAMP_SYMBOL OPEN_PAR_SYMBOL inSumExpr CLOSE_PAR_SYMBOL 
    | name = SUM_SYMBOL OPEN_PAR_SYMBOL DISTINCT_SYMBOL? inSumExpr CLOSE_PAR_SYMBOL 
    | name = GROUP_CONCAT_SYMBOL OPEN_PAR_SYMBOL DISTINCT_SYMBOL? exprList orderClause? (
        SEPARATOR_SYMBOL textString
    )? CLOSE_PAR_SYMBOL
;
textString:
    textStringLiteral
    | HEX_NUMBER
    | BIN_NUMBER
;

inSumExpr:
    ALL_SYMBOL? expr
;

variable:
    userVariable
    | systemVariable
;
arrayCast:
     ARRAY_SYMBOL
;
exprWithParentheses:
    OPEN_PAR_SYMBOL expr CLOSE_PAR_SYMBOL
;
parentheses:
    OPEN_PAR_SYMBOL CLOSE_PAR_SYMBOL
;
orderList:
    orderExpression (COMMA_SYMBOL orderExpression)*
;
orderClause:
    ORDER_SYMBOL BY_SYMBOL orderList
;

orderExpression:
    expr direction?
;
direction:
    ASC_SYMBOL
    | DESC_SYMBOL
;
simpleExprWithParentheses:
    OPEN_PAR_SYMBOL simpleExpr CLOSE_PAR_SYMBOL
;
userVariable:
    AT_SIGN_SYMBOL textOrIdentifier
    | AT_TEXT_SUFFIX
;

systemVariable:
    AT_AT_SIGN_SYMBOL varIdentType? textOrIdentifier dotIdentifier?
;
varIdentType:
    GLOBAL_SYMBOL DOT_SYMBOL
    | LOCAL_SYMBOL DOT_SYMBOL
    | SESSION_SYMBOL DOT_SYMBOL
;
groupingOperation:
    GROUPING_SYMBOL OPEN_PAR_SYMBOL exprList CLOSE_PAR_SYMBOL
;
exprList:
    expr (COMMA_SYMBOL expr)*
;
castType:
    BINARY_SYMBOL fieldLength?
    | CHAR_SYMBOL fieldLength? charsetWithOptBinary?
    | nchar fieldLength?
    | SIGNED_SYMBOL INT_SYMBOL?
    | UNSIGNED_SYMBOL INT_SYMBOL?
    | DATE_SYMBOL
    | TIME_SYMBOL typeDatetimePrecision?
    | DATETIME_SYMBOL typeDatetimePrecision?
    | DECIMAL_SYMBOL floatOptions?
    |  JSON_SYMBOL
    |  realType
    |  FLOAT_SYMBOL standardFloatOptions?
;

floatOptions:
    fieldLength
    | precision
;

standardFloatOptions:
    precision
;

precision:
    OPEN_PAR_SYMBOL INT_NUMBER COMMA_SYMBOL INT_NUMBER CLOSE_PAR_SYMBOL
;

ascii:
    ASCII_SYMBOL BINARY_SYMBOL?
    | BINARY_SYMBOL ASCII_SYMBOL
;

unicode:
    UNICODE_SYMBOL BINARY_SYMBOL?
    | BINARY_SYMBOL UNICODE_SYMBOL
;
textStringLiteral:
    value = SINGLE_QUOTED_TEXT
    |  value = DOUBLE_QUOTED_TEXT
;
charsetName:
    textOrIdentifier
    | BINARY_SYMBOL
    |  DEFAULT_SYMBOL
;
textOrIdentifier:
    identifier
    | textStringLiteral
;
charset:
    CHAR_SYMBOL SET_SYMBOL
    | CHARSET_SYMBOL
;

charsetWithOptBinary:
    ascii
    | unicode
    | BYTE_SYMBOL
    | charset charsetName BINARY_SYMBOL?
    | BINARY_SYMBOL (charset charsetName)?
;

typeDatetimePrecision:
    OPEN_PAR_SYMBOL INT_NUMBER CLOSE_PAR_SYMBOL
;

nchar:
    type = NCHAR_SYMBOL
    | type = NATIONAL_SYMBOL CHAR_SYMBOL
;

realType:
    type = REAL_SYMBOL
    | type = DOUBLE_SYMBOL PRECISION_SYMBOL?
;

fieldLength:
    OPEN_PAR_SYMBOL (real_ulonglong_number | DECIMAL_NUMBER) CLOSE_PAR_SYMBOL
;

real_ulonglong_number:
    INT_NUMBER
    |  HEX_NUMBER
    | ULONGLONG_NUMBER
    | LONG_NUMBER
;

intervalTimeStamp:
    MICROSECOND_SYMBOL
    | SECOND_SYMBOL
    | MINUTE_SYMBOL
    | HOUR_SYMBOL
    | DAY_SYMBOL
    | WEEK_SYMBOL
    | MONTH_SYMBOL
    | QUARTER_SYMBOL
    | YEAR_SYMBOL
;

interval:
    intervalTimeStamp
    | (
        SECOND_MICROSECOND_SYMBOL
        | MINUTE_MICROSECOND_SYMBOL
        | MINUTE_SECOND_SYMBOL
        | HOUR_MICROSECOND_SYMBOL
        | HOUR_SECOND_SYMBOL
        | HOUR_MINUTE_SYMBOL
        | DAY_MICROSECOND_SYMBOL
        | DAY_SECOND_SYMBOL
        | DAY_MINUTE_SYMBOL
        | DAY_HOUR_SYMBOL
        | YEAR_MONTH_SYMBOL
    )
;

identListArg:
    identList
    | OPEN_PAR_SYMBOL identList CLOSE_PAR_SYMBOL
;
identList:
    simpleIdentifier (COMMA_SYMBOL simpleIdentifier)*
;

not2Rule:
    LOGICAL_NOT_OPERATOR
    | NOT2_SYMBOL
;

simpleIdentifier: // simple_ident + simple_ident_q
    identifier (dotIdentifier dotIdentifier?)?
    |  dotIdentifier dotIdentifier
;

dotIdentifier:
     DOT_IDENTIFIER |
    DOT_SYMBOL identifier
;

whenExpression:
    WHEN_SYMBOL expr
;

thenExpression:
    THEN_SYMBOL expr
;

elseExpression:
    ELSE_SYMBOL expr
;

bitExpr:
    simpleExpr
    | bitExpr op = BITWISE_XOR_OPERATOR bitExpr
    | bitExpr op = (
        MULT_OPERATOR
        | DIV_OPERATOR
        | MOD_OPERATOR
        | DIV_SYMBOL
        | MOD_SYMBOL
    ) bitExpr
    | bitExpr op = (PLUS_OPERATOR | MINUS_OPERATOR) bitExpr
    | bitExpr op = (PLUS_OPERATOR | MINUS_OPERATOR) INTERVAL_SYMBOL expr interval
    | bitExpr op = (SHIFT_LEFT_OPERATOR | SHIFT_RIGHT_OPERATOR) bitExpr
    | bitExpr op = BITWISE_AND_OPERATOR bitExpr
    | bitExpr op = BITWISE_OR_OPERATOR bitExpr
;

tableWild:
    identifier DOT_SYMBOL (identifier DOT_SYMBOL)? MULT_OPERATOR
;

// Identifiers excluding keywords (except if they are quoted). IDENT_sys in sql_yacc.yy.
pureIdentifier:
    (IDENTIFIER | BACK_TICK_QUOTED_ID)
    |  DOUBLE_QUOTED_TEXT
;

identifier:
    pureIdentifier
    | identifierKeyword
;

identifierKeyword:
     (
        labelKeyword
        | roleOrIdentifierKeyword
        | EXECUTE_SYMBOL
        |  SHUTDOWN_SYMBOL // Previously allowed as SP label as well.
        |  RESTART_SYMBOL
    )
    | (
        identifierKeywordsUnambiguous
        | identifierKeywordsAmbiguous1RolesAndLabels
        | identifierKeywordsAmbiguous2Labels
        | identifierKeywordsAmbiguous3Roles
        | identifierKeywordsAmbiguous4SystemVariables
    )
;
roleOrIdentifierKeyword:
    (
        ACCOUNT_SYMBOL                  // Conditionally set in the lexer.
        | ASCII_SYMBOL
        | ALWAYS_SYMBOL                 // Conditionally set in the lexer.
        | BACKUP_SYMBOL
        | BEGIN_SYMBOL
        | BYTE_SYMBOL
        | CACHE_SYMBOL
        | CHARSET_SYMBOL
        | CHECKSUM_SYMBOL
        | CLONE_SYMBOL                  // Conditionally set in the lexer.
        | CLOSE_SYMBOL
        | COMMENT_SYMBOL
        | COMMIT_SYMBOL
        | CONTAINS_SYMBOL
        | DEALLOCATE_SYMBOL
        | DO_SYMBOL
        | END_SYMBOL
        | FLUSH_SYMBOL
        | FOLLOWS_SYMBOL
        | FORMAT_SYMBOL
        | GROUP_REPLICATION_SYMBOL      // Conditionally set in the lexer.
        | HANDLER_SYMBOL
        | HELP_SYMBOL
        | HOST_SYMBOL
        | INSTALL_SYMBOL
        | INVISIBLE_SYMBOL              // Conditionally set in the lexer.
        | LANGUAGE_SYMBOL
        | NO_SYMBOL
        | OPEN_SYMBOL
        | OPTIONS_SYMBOL
        | OWNER_SYMBOL
        | PARSER_SYMBOL
        | PARTITION_SYMBOL
        | PORT_SYMBOL
        | PRECEDES_SYMBOL
        | PREPARE_SYMBOL
        | REMOVE_SYMBOL
        | REPAIR_SYMBOL
        | RESET_SYMBOL
        | RESTORE_SYMBOL
        | ROLE_SYMBOL                   // Conditionally set in the lexer.
        | ROLLBACK_SYMBOL
        | SAVEPOINT_SYMBOL
        | SECONDARY_SYMBOL              // Conditionally set in the lexer.
        | SECONDARY_ENGINE_SYMBOL       // Conditionally set in the lexer.
        | SECONDARY_LOAD_SYMBOL         // Conditionally set in the lexer.
        | SECONDARY_UNLOAD_SYMBOL       // Conditionally set in the lexer.
        | SECURITY_SYMBOL
        | SERVER_SYMBOL
        | SIGNED_SYMBOL
        | SOCKET_SYMBOL
        | SLAVE_SYMBOL
        | SONAME_SYMBOL
        | START_SYMBOL
        | STOP_SYMBOL
        | TRUNCATE_SYMBOL
        | UNICODE_SYMBOL
        | UNINSTALL_SYMBOL
        | UPGRADE_SYMBOL
        | VISIBLE_SYMBOL                // Conditionally set in the lexer.
        | WRAPPER_SYMBOL
        | XA_SYMBOL
    )
    // Rules that entered or left this rule in specific versions.
    |  SHUTDOWN_SYMBOL
    |  IMPORT_SYMBOL
;
roleOrLabelKeyword:
    (
        ACTION_SYMBOL
        | ACTIVE_SYMBOL                 // Conditionally set in the lexer.
        | ADDDATE_SYMBOL
        | AFTER_SYMBOL
        | AGAINST_SYMBOL
        | AGGREGATE_SYMBOL
        | ALGORITHM_SYMBOL
        | ANALYSE_SYMBOL                // Conditionally set in the lexer.
        | ANY_SYMBOL
        | AT_SYMBOL
        | AUTHORS_SYMBOL                // Conditionally set in the lexer.
        | AUTO_INCREMENT_SYMBOL
        | AUTOEXTEND_SIZE_SYMBOL
        | AVG_ROW_LENGTH_SYMBOL
        | AVG_SYMBOL
        | BINLOG_SYMBOL
        | BIT_SYMBOL
        | BLOCK_SYMBOL
        | BOOL_SYMBOL
        | BOOLEAN_SYMBOL
        | BTREE_SYMBOL
        | BUCKETS_SYMBOL                // Conditionally set in the lexer.
        | CASCADED_SYMBOL
        | CATALOG_NAME_SYMBOL
        | CHAIN_SYMBOL
        | CHANGED_SYMBOL
        | CHANNEL_SYMBOL                // Conditionally set in the lexer.
        | CIPHER_SYMBOL
        | CLIENT_SYMBOL
        | CLASS_ORIGIN_SYMBOL
        | COALESCE_SYMBOL
        | CODE_SYMBOL
        | COLLATION_SYMBOL
        | COLUMN_NAME_SYMBOL
        | COLUMN_FORMAT_SYMBOL
        | COLUMNS_SYMBOL
        | COMMITTED_SYMBOL
        | COMPACT_SYMBOL
        | COMPLETION_SYMBOL
        | COMPONENT_SYMBOL
        | COMPRESSED_SYMBOL             // Conditionally set in the lexer.
        | COMPRESSION_SYMBOL            // Conditionally set in the lexer.
        | CONCURRENT_SYMBOL
        | CONNECTION_SYMBOL
        | CONSISTENT_SYMBOL
        | CONSTRAINT_CATALOG_SYMBOL
        | CONSTRAINT_SCHEMA_SYMBOL
        | CONSTRAINT_NAME_SYMBOL
        | CONTEXT_SYMBOL
        | CONTRIBUTORS_SYMBOL           // Conditionally set in the lexer.
        | CPU_SYMBOL
        /*
          Although a reserved keyword in SQL:2003 (and :2008),
          not reserved in MySQL per WL#2111 specification.
        */
        | CURRENT_SYMBOL
        | CURSOR_NAME_SYMBOL
        | DATA_SYMBOL
        | DATAFILE_SYMBOL
        | DATETIME_SYMBOL
        | DATE_SYMBOL
        | DAY_SYMBOL
        | DEFAULT_AUTH_SYMBOL
        | DEFINER_SYMBOL
        | DELAY_KEY_WRITE_SYMBOL
        | DES_KEY_FILE_SYMBOL           // Conditionally set in the lexer.
        | DESCRIPTION_SYMBOL            // Conditionally set in the lexer.
        | DIAGNOSTICS_SYMBOL
        | DIRECTORY_SYMBOL
        | DISABLE_SYMBOL
        | DISCARD_SYMBOL
        | DISK_SYMBOL
        | DUMPFILE_SYMBOL
        | DUPLICATE_SYMBOL
        | DYNAMIC_SYMBOL
        | ENCRYPTION_SYMBOL             // Conditionally set in the lexer.
        | ENDS_SYMBOL
        | ENUM_SYMBOL
        | ENGINE_SYMBOL
        | ENGINES_SYMBOL
        | ERROR_SYMBOL
        | ERRORS_SYMBOL
        | ESCAPE_SYMBOL
        | EVENTS_SYMBOL
        | EVERY_SYMBOL
        | EXCLUDE_SYMBOL                // Conditionally set in the lexer.
        | EXPANSION_SYMBOL
        | EXPORT_SYMBOL
        | EXTENDED_SYMBOL
        | EXTENT_SIZE_SYMBOL
        | FAULTS_SYMBOL
        | FAST_SYMBOL
        | FOLLOWING_SYMBOL              // Conditionally set in the lexer.
        | FOUND_SYMBOL
        | ENABLE_SYMBOL
        | FULL_SYMBOL
        | FILE_BLOCK_SIZE_SYMBOL        // Conditionally set in the lexer.
        | FILTER_SYMBOL
        | FIRST_SYMBOL
        | FIXED_SYMBOL
        | GENERAL_SYMBOL
        | GEOMETRY_SYMBOL
        | GEOMETRYCOLLECTION_SYMBOL
        | GET_FORMAT_SYMBOL
        | GRANTS_SYMBOL
        | GLOBAL_SYMBOL
        | HASH_SYMBOL
        | HISTOGRAM_SYMBOL              // Conditionally set in the lexer.
        | HISTORY_SYMBOL                // Conditionally set in the lexer.
        | HOSTS_SYMBOL
        | HOUR_SYMBOL
        | IDENTIFIED_SYMBOL
        | IGNORE_SERVER_IDS_SYMBOL
        | INVOKER_SYMBOL
        | INDEXES_SYMBOL
        | INITIAL_SIZE_SYMBOL
        | INSTANCE_SYMBOL               // Conditionally deprecated in the lexer.
        | INACTIVE_SYMBOL               // Conditionally set in the lexer.
        | IO_SYMBOL
        | IPC_SYMBOL
        | ISOLATION_SYMBOL
        | ISSUER_SYMBOL
        | INSERT_METHOD_SYMBOL
        | JSON_SYMBOL                   // Conditionally set in the lexer.
        | KEY_BLOCK_SIZE_SYMBOL
        | LAST_SYMBOL
        | LEAVES_SYMBOL
        | LESS_SYMBOL
        | LEVEL_SYMBOL
        | LINESTRING_SYMBOL
        | LIST_SYMBOL
        | LOCAL_SYMBOL
        | LOCKED_SYMBOL                 // Conditionally set in the lexer.
        | LOCKS_SYMBOL
        | LOGFILE_SYMBOL
        | LOGS_SYMBOL
        | MAX_ROWS_SYMBOL
        | MASTER_SYMBOL
        | MASTER_HEARTBEAT_PERIOD_SYMBOL
        | MASTER_HOST_SYMBOL
        | MASTER_PORT_SYMBOL
        | MASTER_LOG_FILE_SYMBOL
        | MASTER_LOG_POS_SYMBOL
        | MASTER_USER_SYMBOL
        | MASTER_PASSWORD_SYMBOL
        | MASTER_PUBLIC_KEY_PATH_SYMBOL // Conditionally set in the lexer.
        | MASTER_SERVER_ID_SYMBOL
        | MASTER_CONNECT_RETRY_SYMBOL
        | MASTER_RETRY_COUNT_SYMBOL
        | MASTER_DELAY_SYMBOL
        | MASTER_SSL_SYMBOL
        | MASTER_SSL_CA_SYMBOL
        | MASTER_SSL_CAPATH_SYMBOL
        | MASTER_TLS_VERSION_SYMBOL     // Conditionally deprecated in the lexer.
        | MASTER_SSL_CERT_SYMBOL
        | MASTER_SSL_CIPHER_SYMBOL
        | MASTER_SSL_CRL_SYMBOL
        | MASTER_SSL_CRLPATH_SYMBOL
        | MASTER_SSL_KEY_SYMBOL
        | MASTER_AUTO_POSITION_SYMBOL
        | MAX_CONNECTIONS_PER_HOUR_SYMBOL
        | MAX_QUERIES_PER_HOUR_SYMBOL
        | MAX_STATEMENT_TIME_SYMBOL     // Conditionally deprecated in the lexer.
        | MAX_SIZE_SYMBOL
        | MAX_UPDATES_PER_HOUR_SYMBOL
        | MAX_USER_CONNECTIONS_SYMBOL
        | MEDIUM_SYMBOL
        | MEMORY_SYMBOL
        | MERGE_SYMBOL
        | MESSAGE_TEXT_SYMBOL
        | MICROSECOND_SYMBOL
        | MIGRATE_SYMBOL
        | MINUTE_SYMBOL
        | MIN_ROWS_SYMBOL
        | MODIFY_SYMBOL
        | MODE_SYMBOL
        | MONTH_SYMBOL
        | MULTILINESTRING_SYMBOL
        | MULTIPOINT_SYMBOL
        | MULTIPOLYGON_SYMBOL
        | MUTEX_SYMBOL
        | MYSQL_ERRNO_SYMBOL
        | NAME_SYMBOL
        | NAMES_SYMBOL
        | NATIONAL_SYMBOL
        | NCHAR_SYMBOL
        | NDBCLUSTER_SYMBOL
        | NESTED_SYMBOL                 // Conditionally set in the lexer.
        | NEVER_SYMBOL
        | NEXT_SYMBOL
        | NEW_SYMBOL
        | NO_WAIT_SYMBOL
        | NODEGROUP_SYMBOL
        | NULLS_SYMBOL                  // Conditionally set in the lexer.
        | NOWAIT_SYMBOL                 // Conditionally set in the lexer.
        | NUMBER_SYMBOL
        | NVARCHAR_SYMBOL
        | OFFSET_SYMBOL
        | OLD_SYMBOL                    // Conditionally set in the lexer.
        | OLD_PASSWORD_SYMBOL           // Conditionally set in the lexer.
        | ONE_SYMBOL
        | OPTIONAL_SYMBOL               // Conditionally set in the lexer.
        | ORDINALITY_SYMBOL             // Conditionally set in the lexer.
        | ORGANIZATION_SYMBOL           // Conditionally set in the lexer.
        | OTHERS_SYMBOL                 // Conditionally set in the lexer.
        | PACK_KEYS_SYMBOL
        | PAGE_SYMBOL
        | PARTIAL_SYMBOL
        | PARTITIONING_SYMBOL
        | PARTITIONS_SYMBOL
        | PASSWORD_SYMBOL
        | PATH_SYMBOL                   // Conditionally set in the lexer.
        | PHASE_SYMBOL
        | PLUGIN_DIR_SYMBOL
        | PLUGIN_SYMBOL
        | PLUGINS_SYMBOL
        | POINT_SYMBOL
        | POLYGON_SYMBOL
        | PRECEDING_SYMBOL              // Conditionally set in the lexer.
        | PRESERVE_SYMBOL
        | PREV_SYMBOL
        | THREAD_PRIORITY_SYMBOL        // Conditionally set in the lexer.
        | PRIVILEGES_SYMBOL
        | PROCESSLIST_SYMBOL
        | PROFILE_SYMBOL
        | PROFILES_SYMBOL
        | QUARTER_SYMBOL
        | QUERY_SYMBOL
        | QUICK_SYMBOL
        | READ_ONLY_SYMBOL
        | REBUILD_SYMBOL
        | RECOVER_SYMBOL
        | REDO_BUFFER_SIZE_SYMBOL
        | REDOFILE_SYMBOL               // Conditionally set in the lexer.
        | REDUNDANT_SYMBOL
        | RELAY_SYMBOL
        | RELAYLOG_SYMBOL
        | RELAY_LOG_FILE_SYMBOL
        | RELAY_LOG_POS_SYMBOL
        | RELAY_THREAD_SYMBOL
        | REMOTE_SYMBOL                 // Conditionally set in the lexer.
        | REORGANIZE_SYMBOL
        | REPEATABLE_SYMBOL
        | REPLICATE_DO_DB_SYMBOL
        | REPLICATE_IGNORE_DB_SYMBOL
        | REPLICATE_DO_TABLE_SYMBOL
        | REPLICATE_IGNORE_TABLE_SYMBOL
        | REPLICATE_WILD_DO_TABLE_SYMBOL
        | REPLICATE_WILD_IGNORE_TABLE_SYMBOL
        | REPLICATE_REWRITE_DB_SYMBOL
        | USER_RESOURCES_SYMBOL         // Placed like in the server grammar where it is named just RESOURCES.
        | RESPECT_SYMBOL                // Conditionally set in the lexer.
        | RESUME_SYMBOL
        | RETAIN_SYMBOL                 // Conditionally set in the lexer.
        | RETURNED_SQLSTATE_SYMBOL
        | RETURNS_SYMBOL
        | REUSE_SYMBOL                  // Conditionally set in the lexer.
        | REVERSE_SYMBOL
        | ROLLUP_SYMBOL
        | ROTATE_SYMBOL                 // Conditionally deprecated in the lexer.
        | ROUTINE_SYMBOL
        | ROW_COUNT_SYMBOL
        | ROW_FORMAT_SYMBOL
        | RTREE_SYMBOL
        | SCHEDULE_SYMBOL
        | SCHEMA_NAME_SYMBOL
        | SECOND_SYMBOL
        | SERIAL_SYMBOL
        | SERIALIZABLE_SYMBOL
        | SESSION_SYMBOL
        | SHARE_SYMBOL
        | SIMPLE_SYMBOL
        | SKIP_SYMBOL                   // Conditionally set in the lexer.
        | SLOW_SYMBOL
        | SNAPSHOT_SYMBOL
        | SOUNDS_SYMBOL
        | SOURCE_SYMBOL
        | SQL_AFTER_GTIDS_SYMBOL
        | SQL_AFTER_MTS_GAPS_SYMBOL
        | SQL_BEFORE_GTIDS_SYMBOL
        | SQL_CACHE_SYMBOL              // Conditionally deprecated in the lexer.
        | SQL_BUFFER_RESULT_SYMBOL
        | SQL_NO_CACHE_SYMBOL
        | SQL_THREAD_SYMBOL
        | SRID_SYMBOL                   // Conditionally set in the lexer.
        | STACKED_SYMBOL
        | STARTS_SYMBOL
        | STATS_AUTO_RECALC_SYMBOL
        | STATS_PERSISTENT_SYMBOL
        | STATS_SAMPLE_PAGES_SYMBOL
        | STATUS_SYMBOL
        | STORAGE_SYMBOL
        | STRING_SYMBOL
        | SUBCLASS_ORIGIN_SYMBOL
        | SUBDATE_SYMBOL
        | SUBJECT_SYMBOL
        | SUBPARTITION_SYMBOL
        | SUBPARTITIONS_SYMBOL
        | SUPER_SYMBOL
        | SUSPEND_SYMBOL
        | SWAPS_SYMBOL
        | SWITCHES_SYMBOL
        | TABLE_NAME_SYMBOL
        | TABLES_SYMBOL
        | TABLE_CHECKSUM_SYMBOL
        | TABLESPACE_SYMBOL
        | TEMPORARY_SYMBOL
        | TEMPTABLE_SYMBOL
        | TEXT_SYMBOL
        | THAN_SYMBOL
        | TIES_SYMBOL                   // Conditionally set in the lexer.
        | TRANSACTION_SYMBOL
        | TRIGGERS_SYMBOL
        | TIMESTAMP_SYMBOL
        | TIMESTAMP_ADD_SYMBOL
        | TIMESTAMP_DIFF_SYMBOL
        | TIME_SYMBOL
        | TYPES_SYMBOL
        | TYPE_SYMBOL
        | UDF_RETURNS_SYMBOL
        | UNBOUNDED_SYMBOL              // Conditionally set in the lexer.
        | UNCOMMITTED_SYMBOL
        | UNDEFINED_SYMBOL
        | UNDO_BUFFER_SIZE_SYMBOL
        | UNDOFILE_SYMBOL
        | UNKNOWN_SYMBOL
        | UNTIL_SYMBOL
        | USER_SYMBOL
        | USE_FRM_SYMBOL
        | VARIABLES_SYMBOL
        | VCPU_SYMBOL                   // Conditionally set in the lexer.
        | VIEW_SYMBOL
        | VALUE_SYMBOL
        | WARNINGS_SYMBOL
        | WAIT_SYMBOL
        | WEEK_SYMBOL
        | WORK_SYMBOL
        | WEIGHT_STRING_SYMBOL
        | X509_SYMBOL
        | XID_SYMBOL
        | XML_SYMBOL
        | YEAR_SYMBOL
    )
    // Tokens that entered or left this rule in specific versions and are not automatically
    // handled in the lexer.
    |  SHUTDOWN_SYMBOL
    |  (
        CUBE_SYMBOL
        | IMPORT_SYMBOL
        | FUNCTION_SYMBOL
        | ROWS_SYMBOL
        | ROW_SYMBOL
    )
    |  (
        EXCHANGE_SYMBOL
        | EXPIRE_SYMBOL
        | ONLY_SYMBOL
        | SUPER_SYMBOL
        | VALIDATION_SYMBOL
        | WITHOUT_SYMBOL
    )
    |  ADMIN_SYMBOL
;
labelKeyword:
     (
        roleOrLabelKeyword
        | EVENT_SYMBOL
        | FILE_SYMBOL
        | NONE_SYMBOL
        | PROCESS_SYMBOL
        | PROXY_SYMBOL
        | RELOAD_SYMBOL
        | REPLICATION_SYMBOL
        | RESOURCE_SYMBOL // Conditionally set in the lexer.
        | SUPER_SYMBOL
    )
    | (
        identifierKeywordsUnambiguous
        | identifierKeywordsAmbiguous3Roles
        | identifierKeywordsAmbiguous4SystemVariables
    )
;
identifierKeywordsAmbiguous4SystemVariables:
    GLOBAL_SYMBOL
    | LOCAL_SYMBOL
    | PERSIST_SYMBOL
    | PERSIST_ONLY_SYMBOL
    | SESSION_SYMBOL
;
identifierKeywordsAmbiguous3Roles:
    EVENT_SYMBOL
    | FILE_SYMBOL
    | NONE_SYMBOL
    | PROCESS_SYMBOL
    | PROXY_SYMBOL
    | RELOAD_SYMBOL
    | REPLICATION_SYMBOL
    | RESOURCE_SYMBOL
    | SUPER_SYMBOL
;

identifierKeywordsUnambiguous:
    (
        ACTION_SYMBOL
        | ACCOUNT_SYMBOL
        | ACTIVE_SYMBOL
        | ADDDATE_SYMBOL
        | ADMIN_SYMBOL
        | AFTER_SYMBOL
        | AGAINST_SYMBOL
        | AGGREGATE_SYMBOL
        | ALGORITHM_SYMBOL
        | ALWAYS_SYMBOL
        | ANY_SYMBOL
        | AT_SYMBOL
        | AUTOEXTEND_SIZE_SYMBOL
        | AUTO_INCREMENT_SYMBOL
        | AVG_ROW_LENGTH_SYMBOL
        | AVG_SYMBOL
        | BACKUP_SYMBOL
        | BINLOG_SYMBOL
        | BIT_SYMBOL
        | BLOCK_SYMBOL
        | BOOLEAN_SYMBOL
        | BOOL_SYMBOL
        | BTREE_SYMBOL
        | BUCKETS_SYMBOL
        | CASCADED_SYMBOL
        | CATALOG_NAME_SYMBOL
        | CHAIN_SYMBOL
        | CHANGED_SYMBOL
        | CHANNEL_SYMBOL
        | CIPHER_SYMBOL
        | CLASS_ORIGIN_SYMBOL
        | CLIENT_SYMBOL
        | CLOSE_SYMBOL
        | COALESCE_SYMBOL
        | CODE_SYMBOL
        | COLLATION_SYMBOL
        | COLUMNS_SYMBOL
        | COLUMN_FORMAT_SYMBOL
        | COLUMN_NAME_SYMBOL
        | COMMITTED_SYMBOL
        | COMPACT_SYMBOL
        | COMPLETION_SYMBOL
        | COMPONENT_SYMBOL
        | COMPRESSED_SYMBOL
        | COMPRESSION_SYMBOL
        | CONCURRENT_SYMBOL
        | CONNECTION_SYMBOL
        | CONSISTENT_SYMBOL
        | CONSTRAINT_CATALOG_SYMBOL
        | CONSTRAINT_NAME_SYMBOL
        | CONSTRAINT_SCHEMA_SYMBOL
        | CONTEXT_SYMBOL
        | CPU_SYMBOL
        | CURRENT_SYMBOL // not reserved in MySQL per WL#2111 specification
        | CURSOR_NAME_SYMBOL
        | DATAFILE_SYMBOL
        | DATA_SYMBOL
        | DATETIME_SYMBOL
        | DATE_SYMBOL
        | DAY_SYMBOL
        | DEFAULT_AUTH_SYMBOL
        | DEFINER_SYMBOL
        | DEFINITION_SYMBOL
        | DELAY_KEY_WRITE_SYMBOL
        | DESCRIPTION_SYMBOL
        | DIAGNOSTICS_SYMBOL
        | DIRECTORY_SYMBOL
        | DISABLE_SYMBOL
        | DISCARD_SYMBOL
        | DISK_SYMBOL
        | DUMPFILE_SYMBOL
        | DUPLICATE_SYMBOL
        | DYNAMIC_SYMBOL
        | ENABLE_SYMBOL
        | ENCRYPTION_SYMBOL
        | ENDS_SYMBOL
        | ENFORCED_SYMBOL
        | ENGINES_SYMBOL
        | ENGINE_SYMBOL
        | ENUM_SYMBOL
        | ERRORS_SYMBOL
        | ERROR_SYMBOL
        | ESCAPE_SYMBOL
        | EVENTS_SYMBOL
        | EVERY_SYMBOL
        | EXCHANGE_SYMBOL
        | EXCLUDE_SYMBOL
        | EXPANSION_SYMBOL
        | EXPIRE_SYMBOL
        | EXPORT_SYMBOL
        | EXTENDED_SYMBOL
        | EXTENT_SIZE_SYMBOL
        | FAST_SYMBOL
        | FAULTS_SYMBOL
        | FILE_BLOCK_SIZE_SYMBOL
        | FILTER_SYMBOL
        | FIRST_SYMBOL
        | FIXED_SYMBOL
        | FOLLOWING_SYMBOL
        | FORMAT_SYMBOL
        | FOUND_SYMBOL
        | FULL_SYMBOL
        | GENERAL_SYMBOL
        | GEOMETRYCOLLECTION_SYMBOL
        | GEOMETRY_SYMBOL
        | GET_FORMAT_SYMBOL
        | GET_MASTER_PUBLIC_KEY_SYMBOL
        | GRANTS_SYMBOL
        | GROUP_REPLICATION_SYMBOL
        | HASH_SYMBOL
        | HISTOGRAM_SYMBOL
        | HISTORY_SYMBOL
        | HOSTS_SYMBOL
        | HOST_SYMBOL
        | HOUR_SYMBOL
        | IDENTIFIED_SYMBOL
        | IGNORE_SERVER_IDS_SYMBOL
        | INACTIVE_SYMBOL
        | INDEXES_SYMBOL
        | INITIAL_SIZE_SYMBOL
        | INSERT_METHOD_SYMBOL
        | INSTANCE_SYMBOL
        | INVISIBLE_SYMBOL
        | INVOKER_SYMBOL
        | IO_SYMBOL
        | IPC_SYMBOL
        | ISOLATION_SYMBOL
        | ISSUER_SYMBOL
        | JSON_SYMBOL
        | KEY_BLOCK_SIZE_SYMBOL
        | LAST_SYMBOL
        | LEAVES_SYMBOL
        | LESS_SYMBOL
        | LEVEL_SYMBOL
        | LINESTRING_SYMBOL
        | LIST_SYMBOL
        | LOCKED_SYMBOL
        | LOCKS_SYMBOL
        | LOGFILE_SYMBOL
        | LOGS_SYMBOL
        | MASTER_AUTO_POSITION_SYMBOL
        | MASTER_COMPRESSION_ALGORITHM_SYMBOL
        | MASTER_CONNECT_RETRY_SYMBOL
        | MASTER_DELAY_SYMBOL
        | MASTER_HEARTBEAT_PERIOD_SYMBOL
        | MASTER_HOST_SYMBOL
        | NETWORK_NAMESPACE_SYMBOL
        | MASTER_LOG_FILE_SYMBOL
        | MASTER_LOG_POS_SYMBOL
        | MASTER_PASSWORD_SYMBOL
        | MASTER_PORT_SYMBOL
        | MASTER_PUBLIC_KEY_PATH_SYMBOL
        | MASTER_RETRY_COUNT_SYMBOL
        | MASTER_SERVER_ID_SYMBOL
        | MASTER_SSL_CAPATH_SYMBOL
        | MASTER_SSL_CA_SYMBOL
        | MASTER_SSL_CERT_SYMBOL
        | MASTER_SSL_CIPHER_SYMBOL
        | MASTER_SSL_CRLPATH_SYMBOL
        | MASTER_SSL_CRL_SYMBOL
        | MASTER_SSL_KEY_SYMBOL
        | MASTER_SSL_SYMBOL
        | MASTER_SYMBOL
        | MASTER_TLS_CIPHERSUITES_SYMBOL
        | MASTER_TLS_VERSION_SYMBOL
        | MASTER_USER_SYMBOL
        | MASTER_ZSTD_COMPRESSION_LEVEL_SYMBOL
        | MAX_CONNECTIONS_PER_HOUR_SYMBOL
        | MAX_QUERIES_PER_HOUR_SYMBOL
        | MAX_ROWS_SYMBOL
        | MAX_SIZE_SYMBOL
        | MAX_UPDATES_PER_HOUR_SYMBOL
        | MAX_USER_CONNECTIONS_SYMBOL
        | MEDIUM_SYMBOL
        | MEMORY_SYMBOL
        | MERGE_SYMBOL
        | MESSAGE_TEXT_SYMBOL
        | MICROSECOND_SYMBOL
        | MIGRATE_SYMBOL
        | MINUTE_SYMBOL
        | MIN_ROWS_SYMBOL
        | MODE_SYMBOL
        | MODIFY_SYMBOL
        | MONTH_SYMBOL
        | MULTILINESTRING_SYMBOL
        | MULTIPOINT_SYMBOL
        | MULTIPOLYGON_SYMBOL
        | MUTEX_SYMBOL
        | MYSQL_ERRNO_SYMBOL
        | NAMES_SYMBOL
        | NAME_SYMBOL
        | NATIONAL_SYMBOL
        | NCHAR_SYMBOL
        | NDBCLUSTER_SYMBOL
        | NESTED_SYMBOL
        | NEVER_SYMBOL
        | NEW_SYMBOL
        | NEXT_SYMBOL
        | NODEGROUP_SYMBOL
        | NOWAIT_SYMBOL
        | NO_WAIT_SYMBOL
        | NULLS_SYMBOL
        | NUMBER_SYMBOL
        | NVARCHAR_SYMBOL
        | OFFSET_SYMBOL
        | OJ_SYMBOL
        | OLD_SYMBOL
        | ONE_SYMBOL
        | ONLY_SYMBOL
        | OPEN_SYMBOL
        | OPTIONAL_SYMBOL
        | OPTIONS_SYMBOL
        | ORDINALITY_SYMBOL
        | ORGANIZATION_SYMBOL
        | OTHERS_SYMBOL
        | OWNER_SYMBOL
        | PACK_KEYS_SYMBOL
        | PAGE_SYMBOL
        | PARSER_SYMBOL
        | PARTIAL_SYMBOL
        | PARTITIONING_SYMBOL
        | PARTITIONS_SYMBOL
        | PASSWORD_SYMBOL
        | PATH_SYMBOL
        | PHASE_SYMBOL
        | PLUGINS_SYMBOL
        | PLUGIN_DIR_SYMBOL
        | PLUGIN_SYMBOL
        | POINT_SYMBOL
        | POLYGON_SYMBOL
        | PORT_SYMBOL
        | PRECEDING_SYMBOL
        | PRESERVE_SYMBOL
        | PREV_SYMBOL
        | PRIVILEGES_SYMBOL
        | PRIVILEGE_CHECKS_USER_SYMBOL
        | PROCESSLIST_SYMBOL
        | PROFILES_SYMBOL
        | PROFILE_SYMBOL
        | QUARTER_SYMBOL
        | QUERY_SYMBOL
        | QUICK_SYMBOL
        | READ_ONLY_SYMBOL
        | REBUILD_SYMBOL
        | RECOVER_SYMBOL
        | REDO_BUFFER_SIZE_SYMBOL
        | REDUNDANT_SYMBOL
        | REFERENCE_SYMBOL
        | RELAY_SYMBOL
        | RELAYLOG_SYMBOL
        | RELAY_LOG_FILE_SYMBOL
        | RELAY_LOG_POS_SYMBOL
        | RELAY_THREAD_SYMBOL
        | REMOVE_SYMBOL
        | REORGANIZE_SYMBOL
        | REPEATABLE_SYMBOL
        | REPLICATE_DO_DB_SYMBOL
        | REPLICATE_DO_TABLE_SYMBOL
        | REPLICATE_IGNORE_DB_SYMBOL
        | REPLICATE_IGNORE_TABLE_SYMBOL
        | REPLICATE_REWRITE_DB_SYMBOL
        | REPLICATE_WILD_DO_TABLE_SYMBOL
        | REPLICATE_WILD_IGNORE_TABLE_SYMBOL
        | USER_RESOURCES_SYMBOL
        | RESPECT_SYMBOL
        | RESTORE_SYMBOL
        | RESUME_SYMBOL
        | RETAIN_SYMBOL
        | RETURNED_SQLSTATE_SYMBOL
        | RETURNS_SYMBOL
        | REUSE_SYMBOL
        | REVERSE_SYMBOL
        | ROLE_SYMBOL
        | ROLLUP_SYMBOL
        | ROTATE_SYMBOL
        | ROUTINE_SYMBOL
        | ROW_COUNT_SYMBOL
        | ROW_FORMAT_SYMBOL
        | RTREE_SYMBOL
        | SCHEDULE_SYMBOL
        | SCHEMA_NAME_SYMBOL
        | SECONDARY_ENGINE_SYMBOL
        | SECONDARY_LOAD_SYMBOL
        | SECONDARY_SYMBOL
        | SECONDARY_UNLOAD_SYMBOL
        | SECOND_SYMBOL
        | SECURITY_SYMBOL
        | SERIALIZABLE_SYMBOL
        | SERIAL_SYMBOL
        | SERVER_SYMBOL
        | SHARE_SYMBOL
        | SIMPLE_SYMBOL
        | SKIP_SYMBOL
        | SLOW_SYMBOL
        | SNAPSHOT_SYMBOL
        | SOCKET_SYMBOL
        | SONAME_SYMBOL
        | SOUNDS_SYMBOL
        | SOURCE_SYMBOL
        | SQL_AFTER_GTIDS_SYMBOL
        | SQL_AFTER_MTS_GAPS_SYMBOL
        | SQL_BEFORE_GTIDS_SYMBOL
        | SQL_BUFFER_RESULT_SYMBOL
        | SQL_NO_CACHE_SYMBOL
        | SQL_THREAD_SYMBOL
        | SRID_SYMBOL
        | STACKED_SYMBOL
        | STARTS_SYMBOL
        | STATS_AUTO_RECALC_SYMBOL
        | STATS_PERSISTENT_SYMBOL
        | STATS_SAMPLE_PAGES_SYMBOL
        | STATUS_SYMBOL
        | STORAGE_SYMBOL
        | STRING_SYMBOL
        | SUBCLASS_ORIGIN_SYMBOL
        | SUBDATE_SYMBOL
        | SUBJECT_SYMBOL
        | SUBPARTITIONS_SYMBOL
        | SUBPARTITION_SYMBOL
        | SUSPEND_SYMBOL
        | SWAPS_SYMBOL
        | SWITCHES_SYMBOL
        | TABLES_SYMBOL
        | TABLESPACE_SYMBOL
        | TABLE_CHECKSUM_SYMBOL
        | TABLE_NAME_SYMBOL
        | TEMPORARY_SYMBOL
        | TEMPTABLE_SYMBOL
        | TEXT_SYMBOL
        | THAN_SYMBOL
        | THREAD_PRIORITY_SYMBOL
        | TIES_SYMBOL
        | TIMESTAMP_ADD_SYMBOL
        | TIMESTAMP_DIFF_SYMBOL
        | TIMESTAMP_SYMBOL
        | TIME_SYMBOL
        | TRANSACTION_SYMBOL
        | TRIGGERS_SYMBOL
        | TYPES_SYMBOL
        | TYPE_SYMBOL
        | UNBOUNDED_SYMBOL
        | UNCOMMITTED_SYMBOL
        | UNDEFINED_SYMBOL
        | UNDOFILE_SYMBOL
        | UNDO_BUFFER_SIZE_SYMBOL
        | UNKNOWN_SYMBOL
        | UNTIL_SYMBOL
        | UPGRADE_SYMBOL
        | USER_SYMBOL
        | USE_FRM_SYMBOL
        | VALIDATION_SYMBOL
        | VALUE_SYMBOL
        | VARIABLES_SYMBOL
        | VCPU_SYMBOL
        | VIEW_SYMBOL
        | VISIBLE_SYMBOL
        | WAIT_SYMBOL
        | WARNINGS_SYMBOL
        | WEEK_SYMBOL
        | WEIGHT_STRING_SYMBOL
        | WITHOUT_SYMBOL
        | WORK_SYMBOL
        | WRAPPER_SYMBOL
        | X509_SYMBOL
        | XID_SYMBOL
        | XML_SYMBOL
        | YEAR_SYMBOL
    )
    |  (
        ARRAY_SYMBOL
        | FAILED_LOGIN_ATTEMPTS_SYMBOL
        | MASTER_COMPRESSION_ALGORITHM_SYMBOL
        | MASTER_TLS_CIPHERSUITES_SYMBOL
        | MASTER_ZSTD_COMPRESSION_LEVEL_SYMBOL
        | MEMBER_SYMBOL
        | OFF_SYMBOL
        | PASSWORD_LOCK_TIME_SYMBOL
        | PRIVILEGE_CHECKS_USER_SYMBOL
        | RANDOM_SYMBOL
        | REQUIRE_ROW_FORMAT_SYMBOL
        | REQUIRE_TABLE_PRIMARY_KEY_CHECK_SYMBOL
        | STREAM_SYMBOL
        | TIMESTAMP_SYMBOL
        | TIME_SYMBOL
    )
;

// These non-reserved words cannot be used as role names and SP label names:
identifierKeywordsAmbiguous1RolesAndLabels:
    EXECUTE_SYMBOL
    | RESTART_SYMBOL
    | SHUTDOWN_SYMBOL
;

// These non-reserved keywords cannot be used as unquoted SP label names:
identifierKeywordsAmbiguous2Labels:
    ASCII_SYMBOL
    | BEGIN_SYMBOL
    | BYTE_SYMBOL
    | CACHE_SYMBOL
    | CHARSET_SYMBOL
    | CHECKSUM_SYMBOL
    | CLONE_SYMBOL
    | COMMENT_SYMBOL
    | COMMIT_SYMBOL
    | CONTAINS_SYMBOL
    | DEALLOCATE_SYMBOL
    | DO_SYMBOL
    | END_SYMBOL
    | FLUSH_SYMBOL
    | FOLLOWS_SYMBOL
    | HANDLER_SYMBOL
    | HELP_SYMBOL
    | IMPORT_SYMBOL
    | INSTALL_SYMBOL
    | LANGUAGE_SYMBOL
    | NO_SYMBOL
    | PRECEDES_SYMBOL
    | PREPARE_SYMBOL
    | REPAIR_SYMBOL
    | RESET_SYMBOL
    | ROLLBACK_SYMBOL
    | SAVEPOINT_SYMBOL
    | SIGNED_SYMBOL
    | SLAVE_SYMBOL
    | START_SYMBOL
    | STOP_SYMBOL
    | TRUNCATE_SYMBOL
    | UNICODE_SYMBOL
    | UNINSTALL_SYMBOL
    | XA_SYMBOL
;
