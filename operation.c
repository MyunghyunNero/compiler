#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#define MAX_LEN 256 

typedef enum { PLUS , MINUS, STAR , DIVISOR , LPAREN , RPAREN , INT ,FLOAT_NUM, ENDTOKEN} Token;
typedef enum { NUMBER , OPER ,ERROR ,BLANK , DOT , END } CLASSIFICATION;
typedef enum {INTEGER , FLOAT } NUMBER_TYPE;

typedef union number_t{
    int int_val;
    double float_val;
} number;

typedef struct numberInfo_t {
    NUMBER_TYPE type;
    number data;
} numInfo;

Token token;  //현재 어떤 토큰인지
char nextChar; //다음 문자
CLASSIFICATION classificaiton; //다음 문자가 어떤 분류인지
char lexeme[MAX_LEN];
int idx = 0; //lexeme index 가리킴
bool error = false;  // 입력값 읽으면서 잘못된 입력이 있는지 체크

void clearLexeme();
void getChar();
void getNonBlankChar();
void addChar();
void get_token();
numInfo expression();
numInfo term();
numInfo factor();

//읽은 토큰 문자열 초기화
void clearLexeme(){
    memset(lexeme,0,MAX_LEN); // 문자열 초기화 
    idx=0; // 문자열 인덱스 초기화-> 처음부터 값 들어가게 
}

// 다음 글자가 어떤 분류인지 -> 숫자인지 , 연산자인지 (필요한 이유 : 정수와 실수는 문자열이 여러개이므로 다음 문자가 무엇인지 알아야함)
void getChar(){
    nextChar=getchar();
    if(nextChar>='0' && nextChar <='9'){
        classificaiton = NUMBER;
    }else if(nextChar=='*' || nextChar == '/' || nextChar == '+' || nextChar == '-' || nextChar=='(' || nextChar ==')'){
        classificaiton = OPER;
    }else if(nextChar ==' '){
        classificaiton = BLANK;
    }else if(nextChar == '.'){
        classificaiton = DOT;
    }else if(nextChar == '\n' || nextChar ==EOF){
        classificaiton = END;
    }else{
        classificaiton = ERROR;
    }
}
//빈칸 없이 다음 글자 읽기
void getNonBlankChar(){
    getChar();
    while(classificaiton==BLANK){
        getChar();
    }
}

//문자열에 읽은 문자를 저장한다
void addChar(){
    lexeme[idx++] = nextChar;
}

//읽은 문자가 어떤 토큰인지 알려주는 메서드 
void get_token(){
    clearLexeme();
    
    switch (classificaiton)
    {
        case NUMBER :
            addChar();
            getNonBlankChar();
            while(classificaiton == NUMBER){    // 정수가 나오면 계속 읽기
                addChar();
                getNonBlankChar();
            }
            if(classificaiton == DOT){ //. 으로 끝나면 실수 형일 가능성
                addChar();
                getNonBlankChar();
                while(classificaiton == NUMBER){  // 소수점 숫자 읽기
                    addChar();
                    getNonBlankChar();
                }
                token = FLOAT_NUM;  
            }else{
                token = INT;
            }
            break;

        case OPER : 
            if(nextChar == '+'){
                token = PLUS;
            }else if(nextChar == '-'){
                token = MINUS;
            }else if(nextChar == '*'){
                token = STAR;
            }else if(nextChar == '/'){
                token = DIVISOR;
            }else if(nextChar == '('){
                token = LPAREN;
            }else if(nextChar == ')'){
                token = RPAREN;
            }
            addChar();
            getNonBlankChar();
            break;

        case END :
            token = ENDTOKEN;
            break;

        case ERROR :
            printf("wrong input\n");
            error  = true;
            break;
        case BLANK :
            error  = true;
            break;

        case DOT :
            error  = true;
            break;
    }
}

//expression -> term (+- term)
numInfo expression(){
    numInfo left = term();
    numInfo result = left;
    while(token == PLUS || token ==MINUS ){
        if(token ==PLUS){
            get_token();
            numInfo right = term();
            if(left.type == FLOAT){ 
                if(right.type == INTEGER){
                    printf("warning float + int\n");
                    result = (numInfo){.type=FLOAT , .data.float_val = left.data.float_val + right.data.int_val};
                }else{
                    result = (numInfo){.type=FLOAT , .data.float_val = left.data.float_val + right.data.float_val};
                }
            }
            if(left.type == INTEGER){
                if(right.type == FLOAT){
                    printf("warning int + float\n");
                    result = (numInfo){.type=FLOAT , .data.float_val = left.data.int_val + right.data.float_val};
                }else{
                    result = (numInfo){.type=INTEGER , .data.int_val = left.data.int_val + right.data.int_val};
                }
            }
        }else if(token == MINUS){
            get_token();
            numInfo right = term();
            if(left.type == FLOAT){
                if(right.type == INTEGER){
                    printf("warning float - int\n");
                    result = (numInfo){.type=FLOAT , .data.float_val = left.data.float_val - right.data.int_val};
                }else{
                    result = (numInfo){.type=FLOAT , .data.float_val = left.data.float_val - right.data.float_val};
                }
            }
            if(left.type == INTEGER){
                if(right.type == FLOAT){
                    printf("warning int - float\n");
                    result = (numInfo){.type=FLOAT , .data.float_val = left.data.int_val - right.data.float_val};
                }else{
                    result = (numInfo){.type=INTEGER , .data.int_val = left.data.int_val - right.data.int_val};
                }
            }
        }
    }
    return result;
}

//term -> factor (*/ factor)
numInfo term(){
    numInfo left = factor();
    numInfo result = left;
    while(token == STAR || token == DIVISOR){
        if(token ==STAR){
            get_token();
            numInfo right = factor();
            if(left.type == FLOAT){
                if(right.type == INTEGER){
                    printf("warning float * int\n");
                    result = (numInfo){.type=FLOAT , .data.float_val = left.data.float_val * right.data.int_val};
                }else{
                    result = (numInfo){.type=FLOAT , .data.float_val = left.data.float_val * right.data.float_val};
                }
            }
            if(left.type == INTEGER){
                if(right.type == FLOAT){
                    printf("warning int * float\n");
                    result = (numInfo){.type=FLOAT , .data.float_val = left.data.int_val * right.data.float_val};
                }else{
                    result = (numInfo){.type=INTEGER , .data.int_val = left.data.int_val * right.data.int_val};
                }
            }
        }else if(token == DIVISOR){
            get_token();
            numInfo right = factor();
            if(left.type == FLOAT){
                if(right.type == INTEGER){
                    printf("warning float / int\n");
                    result = (numInfo){.type=FLOAT , .data.float_val = left.data.float_val / right.data.int_val};
                }else{
                    result = (numInfo){.type=FLOAT , .data.float_val = left.data.float_val / right.data.float_val};
                }
            }
            if(left.type == INTEGER){
                if(right.type == FLOAT){
                    printf("warning int / float\n");
                    result = (numInfo){.type=FLOAT , .data.float_val = left.data.int_val / right.data.float_val};
                }else{
                    result = (numInfo){.type=INTEGER , .data.int_val = left.data.int_val / right.data.int_val};
                }
            }
        }
    }
    return result;
}

//factor -> num | (expression)
numInfo factor(){
    if(token == FLOAT_NUM){
        double f = atof(lexeme);
        get_token();
        return (numInfo){.type = FLOAT , .data.float_val = f};
    }else if(token == INT){
        int i = atoi(lexeme);
        printf("factor %d",i);
        get_token();
        return (numInfo) {.type = INTEGER, .data.int_val = i};
    }else if(token == LPAREN){
        get_token();
        numInfo ret = expression();
        if(token == RPAREN){
            get_token();
            return ret;
        }
        else{
            error= true;
            exit(1);
        }
    }else{
        error = true;
        exit(1);
    }
}
int main(){
    getNonBlankChar();
    get_token();
    numInfo result = expression();
    if(token!=ENDTOKEN){
        error = true;
    }
    if(error){
        printf("failed...\n");
        // exit(1);
    }
    if(result.type == INTEGER){
        printf("%d\n",result.data.int_val);
    }else{
        printf("%f\n",result.data.float_val);
    }
    return 0;
}