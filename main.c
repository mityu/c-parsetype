#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define errorOnParse(v, i) errorWithTokensLeft(__FILE__, __LINE__, (v), (i))
#define error() \
    do {fprintf(stderr, "%s:%d: error\n", __FILE__, __LINE__); exit(1);} while(0)
#define safeFree(p) do {if (p) {free(p); (p) = NULL;}} while(0)

typedef enum {
    TokenXXX,
    TokenVoid,
    TokenInt,
    TokenChar,
    TokenFloat,
    TokenAsterisk,
    TokenComma,
    TokenLParen,
    TokenRParen,
    // TokenEOL,
} TokenKind;

typedef struct TokenVec TokenVec;
struct TokenVec {
    TokenKind *head;
    int size;
    int cap;
};

typedef enum {
    TypeVoid,
    TypeInt,
    TypeChar,
    TypeFloat,
    TypePointer,
    TypeFunction,
} TypeKind;

typedef struct Type Type;
typedef struct TypeList TypeList;
struct Type {
    TypeKind kind;
    Type *baseType;  // Valid when kind is TypePointer
    TypeList *argsType;  // Valid when kind is TypeFunction
    Type *retType;  // Valid when kind is TypeFunction
};
struct TypeList {
    TypeList *next;
    Type *type;
};

typedef struct String String;
struct String {
    char *string;
    size_t size;
    size_t cap;
};

void *safeAlloc(size_t size) {
    void *mem = calloc(1, size);
    if (!mem) {
        fputs("calloc() failed", stderr);
        exit(1);
    }
    return mem;
}

TokenVec *newTokenVec(void) {
    TokenVec *obj = (TokenVec *)safeAlloc(sizeof(TokenVec));
    obj->size = 0;
    obj->cap = 16;
    obj->head = (TokenKind *)safeAlloc(obj->cap * sizeof(TokenKind));
    return obj;
}

void freeTokenVec(TokenVec **v) {
    safeFree((*v)->head);
    safeFree(*v);
}

void appendToken(TokenVec *v, TokenKind kind) {
    if (v->size == v->cap) {
        TokenKind *tokenSave = v->head;
        v->cap <<= 1;
        v->head = (TokenKind *)safeAlloc(v->cap * sizeof(TokenKind));
        memcpy(v->head, tokenSave, v->size * sizeof(TokenKind));
        free(tokenSave);
    }
    v->head[v->size++] = kind;
}

const char *tokenToString(TokenKind token) {
    switch (token) {
        case TokenXXX:
            return "XXX";
        case TokenVoid:
            return "void";
        case TokenInt:
            return "int";
        case TokenChar:
            return "char";
        case TokenFloat:
            return "float";
        case TokenAsterisk:
            return "*";
        case TokenComma:
            return ",";
        case TokenLParen:
            return "(";
        case TokenRParen:
            return ")";
        default:
            fputs("Unreachable", stderr);
            exit(1);
    }
}

Type *newType(TypeKind kind) {
    Type *type = (Type *)safeAlloc(sizeof(Type));
    type->kind = kind;
    return type;
}

void freeTypeList(TypeList **tl);

void freeType(Type **type) {
    if (!(*type))
        return;
    if ((*type)->baseType)
        freeType(&(*type)->baseType);
    if ((*type)->retType)
        freeType(&(*type)->retType);
    freeTypeList(&(*type)->argsType);
    safeFree(*type);
}

void freeTypeList(TypeList **tl) {
    if (!(*tl))
        return;
    if ((*tl)->next)
        freeTypeList(&(*tl)->next);
    freeType(&(*tl)->type);
    safeFree(*tl);
}

String *newString(void) {
    String *obj = (String *)safeAlloc(sizeof(String));
    obj->size = 0;
    obj->cap = 128;
    obj->string = (char *)safeAlloc(obj->cap);
    obj->string[0] = '\0';
    return obj;
}

void freeString(String **s) {
    safeFree((*s)->string);
    safeFree(*s);
}

void appendString(String *s, const char *append) {
    size_t len = strlen(append);
    if ((s->size + len) >= s->cap) {
        char *stringSave = s->string;
        s->cap <<= 1;
        s->string = (char *)safeAlloc(s->cap);
        memcpy(s->string, stringSave, s->size);
        free(stringSave);
    }
    memcpy(&s->string[s->size], append, len);
    s->size += len;
    s->string[s->size] = '\0';
}

void trimSuffSpaces(String *s) {
    int i = s->size - 1;
    while (i >= 0 && s->string[i] == ' ')
        --i;
    s->size = i + 1;
    s->string[s->size] = '\0';
}

int isAlpha(char c) {
    return 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z';
}

int matchIdent(const char **p, const char *ident) {
    int len = strlen(ident);
    if (strncmp(*p, ident, len) == 0 && !isAlpha((*p)[len])) {
        *p += len - 1;
        return 1;
    }
    return 0;
}

TokenVec *tokenize(const char *code) {
    TokenVec *v = newTokenVec();
    const char *p = code;

    for (;*p; ++p) {
        if (*p == ' ') {
            continue;
        } else if (*p == '*') {
            appendToken(v, TokenAsterisk);
        } else if (*p == '(') {
            appendToken(v, TokenLParen);
        } else if (*p == ')') {
            appendToken(v, TokenRParen);
        } else if (*p == ',') {
            appendToken(v, TokenComma);
        } else if (matchIdent(&p, "XXX")) {
            appendToken(v, TokenXXX);
        } else if (matchIdent(&p, "void")) {
            appendToken(v, TokenVoid);
        } else if (matchIdent(&p, "int")) {
            appendToken(v, TokenInt);
        } else if (matchIdent(&p, "char")) {
            appendToken(v, TokenChar);
        } else if (matchIdent(&p, "float")) {
            appendToken(v, TokenFloat);
        } else {
            fprintf(stderr, "Cannot tokenize: %s\n", p);
            exit(1);
        }
    }

    return v;
}

Type *getTypeFromToken(TokenKind kind) {
    switch (kind) {
    case TokenVoid:
        return newType(TypeVoid);
    case TokenInt:
        return newType(TypeInt);
    case TokenChar:
        return newType(TypeChar);
    case TokenFloat:
        return newType(TypeFloat);
    default:
        return NULL;
    }
}

void errorWithTokensLeft(const char *file, int line, TokenVec *v, int i) {
    String *s = newString();
    for (; i < v->size; ++i)
        appendString(s, tokenToString(v->head[i]));
    fprintf(stderr, "%s:%d: error: %s\n", file, line, s->string);
    free(s->string);
    free(s);
    exit(1);
}

int isFunctionPointer(Type *type) {
    while (type->kind == TypePointer)
        type = type->baseType;
    return type->kind == TypeFunction;
}

Type *parseBaseType(TokenVec *v, int *index);
Type *parseMainType(TokenVec *v, int *index, Type *baseType);
TypeList *parseFuncArgTypes(TokenVec *v, int *index);

Type *parseType(TokenVec *v, int *i) {
    Type *baseType = parseBaseType(v, i);
    Type *mainType = parseMainType(v, i, baseType);
    if (!mainType)
        mainType = baseType;
    return mainType;
}

Type *parseBaseType(TokenVec *v, int *index) {
    Type *type = getTypeFromToken(v->head[*index]);
    if (!type)
        errorOnParse(v, *index);
    (*index)++;
    return type;
}

Type *parseMainType(TokenVec *v, int *index, Type *baseType) {
    Type *pointerType = baseType;
    Type *mainType = NULL;
    int i = *index;

    while (v->head[i] == TokenAsterisk) {
        Type *tmp = newType(TypePointer);
        tmp->baseType = pointerType;
        pointerType = tmp;
        i++;
    }

    if (v->head[i] == TokenXXX) {
        i++;
    } else if (v->head[i] == TokenLParen) {
        i++;
        if (v->head[i] != TokenAsterisk)
            errorOnParse(v, i);
        mainType = parseMainType(v, &i, pointerType);
        if (v->head[i] != TokenRParen)
            errorOnParse(v, i);
        i++;
    }

    if (!mainType)
        mainType = pointerType;

    if (i >= v->size) {
        *index = i;
        return mainType;
    }

    if (v->head[i] == TokenLParen) {
        static const Type zeroType = {};
        Type *save = newType(pointerType->kind);
        *save = *pointerType;
        *pointerType = zeroType;
        pointerType->kind = TypeFunction;
        pointerType->argsType = parseFuncArgTypes(v, &i);
        pointerType->retType = save;
    }

    *index = i;
    return mainType;
}

TypeList *parseFuncArgTypes(TokenVec *v, int *index) {
    int i = *index;
    TypeList head;
    TypeList *types = &head;
    head.next = NULL;

    if (v->head[i++] != TokenLParen)
        errorOnParse(v, i);

    for (;;) {
        types->next = (TypeList *)safeAlloc(sizeof(TypeList));
        types = types->next;
        types->type = parseType(v, &i);

        if (v->head[i] == TokenComma)
            i++;
        else
            break;
    }

    if (v->head[i++] != TokenRParen)
        errorOnParse(v, i);

    *index = i;
    return head.next;
}

void buildTypeString(const Type *type, String *s) {
    switch (type->kind) {
    case TypeVoid:
        appendString(s, "void ");
        break;
    case TypeInt:
        appendString(s, "int ");
        break;
    case TypeChar:
        appendString(s, "char ");
        break;
    case TypeFloat:
        appendString(s, "float ");
        break;
    case TypePointer:
        appendString(s, "*");
        if (!type->baseType)
            error();
        buildTypeString(type->baseType, s);
        break;
    case TypeFunction:
        appendString(s, "func(");
        if (type->argsType) {
            TypeList *tl = type->argsType;
            for (;;) {
                if (!tl->type)
                    error();
                buildTypeString(tl->type, s);
                if (!tl->next)
                    break;
                trimSuffSpaces(s);
                appendString(s, ", ");
                tl = tl->next;
            }
        }
        trimSuffSpaces(s);
        appendString(s, ") ");
        if (!type->retType)
            error();
        buildTypeString(type->retType, s);
        break;
    default:
        fprintf(stderr, "%s:%d: unreachable", __FILE__, __LINE__);
        exit(1);
    }
}

String *parse(const char *code) {
    int i = 0;
    String *s = newString();
    TokenVec *v;
    Type *type;

    v = tokenize(code);
    type = parseType(v, &i);
    freeTokenVec(&v);
    if (!type) {
        fputs("Failed to parse\n", stderr);
        return s;
    }

    buildTypeString(type, s);
    trimSuffSpaces(s);
    return s;
}

void assert(const char *in, const char *out) {
    String *result = parse(in);
    printf("\"%s\" => ", in);
    if (strcmp(result->string, out) == 0) {
        puts(result->string);
    } else {
        printf("%s ... not ok: Expect, Got:\n", result->string);
        printf("  - %s\n", out);
        printf("  - %s\n", result->string);
    }
    freeString(&result);
}

int main(void) {
    assert("int XXX", "int");
    assert("int XXX(void)", "func(void) int");
    assert("int *XXX", "*int");
    assert("int *XXX(void)", "func(void) *int");
    assert("int (*XXX)(char)", "*func(char) int");
    assert("int (**XXX)(char)", "**func(char) int");
    assert("int (*XXX)(float, char (*)(int, int *))", "*func(float, *func(int, *int) char) int");
    assert("int (*XXX(void))(char)", "func(void) *func(char) int");
    assert("int *(*XXX(void))(char)", "func(void) *func(char) *int");
    assert("int *(*XXX(float (*)(int)))(char)", "func(*func(int) float) *func(char) *int");
}
