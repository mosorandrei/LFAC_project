typedef struct Var {
    char name[100];
    char type[100];
    int type_int;
    char scope_lvl[100];
    bool isConst;
    bool isDecl;
    bool isInit;
    bool isArray;
    int intVal;
    bool boolVal;
    char charVal;
    char strVal[100];
    float floatVal;
} Var;
typedef struct Func {
    char name[100];
    char type_ret[100];
    char type_ret_int;
    int paramsNum;
    bool isDecl;
    bool isInit;
    int intVal;
    bool boolVal;
    char charVal;
    char strVal[100];
    float floatVal;
} Func;
typedef struct Expr {
    int type_int;
    int intVal;
    bool boolVal;
    char charVal;
    float floatVal;
    char* strVal;
} Expr;