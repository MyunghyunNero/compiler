#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#define MAX_LEN 256 

typedef enum { PLUS , MINUS, STAR , DIVISOR , LPAREN , RPAREN ,INTEGER , FLOAT , END} Token;
typedef enum { NUMBER , OPER ,ERROR ,BLANK , DOT , END } CLASSIFICATION;
typedef enum {INTEGER , FLOAT } NUMBER_TYPE;
typedef union number{
    int int_val;
    double float_val;
} number;

typedef struct number {
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
char getNonBlankChar();
void addChar();
void get_token();


void clearLexeme(){
    memset(lexeme,0,MAX_LEN); // 문자열 초기화 
    idx=0; // 문자열 인덱스 초기화-> 처음부터 값 들어가게 
}

// 다음 글자가 어떤 분류인지 -> 숫자인지 , 연산자인지 (필요한 이유 : 정수와 실수는 문자열이 여러개이므로 다음 문자가 무엇인지 알아야함)
void getChar(){
    nextChar=getchar();
    printf("%c\n",nextChar);
    if(nextChar>='0' && nextChar <='9'){
        classificaiton = NUMBER;
    }else if(nextChar=='*' || nextChar == '/' || nextChar == '+' || nextChar == '-' || nextChar=='(' || nextChar ==')'){
        classificaiton = OPER;
    }else if(nextChar ==' '){
        classificaiton = BLANK;
    }else if(nextChar == '.'){
        classificaiton = DOT;
    }else if(nextChar == '\n'){
        classificaiton = END;
    }else{
        classificaiton = ERROR;
    }
}
//빈칸 없이 다음 글자 읽기
char getNonBlankChar(){
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
        while(classificaiton == NUMBER){
            addChar();
            getNonBlankChar();
        }
        if(classificaiton == DOT){
            addChar();
            getNonBlankChar();
            while(classificaiton == NUMBER){
                addChar();
                getNonBlankChar();
            }
            token = FLOAT;
        }else{
            token = INTEGER;
        }
        break;

    case OPER : 
        addChar();
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
        getNonBlankChar();
        break;

    case END :
        token = END;
        break;

    case ERROR :
        printf("wrong input\n");
        error  = true;
        break;
    }
}

numInfo expression(){
    numInfo left = term();
    while(token == PLUS || token ==MINUS ){
        if(token ==PLUS){
            get_token();
            numInfo right = term();
            if(left.type == FLOAT){
                if(right.type == INTEGER){
                    printf("warning float * int\n");
                    return (numInfo){.type=FLOAT , .data.float_val = left.data.float_val + (double)right.data.int_val};
                }else{
                    return (numInfo){.type=FLOAT , .data.float_val = left.data.float_val + right.data.float_val};
                }
            }
            if(left.type == INTEGER){
                if(right.type == FLOAT){
                    printf("warning int * float\n");
                    return (numInfo){.type=FLOAT , .data.float_val = (double)left.data.int_val + right.data.float_val};
                }else{
                    return (numInfo){.type=INTEGER , .data.int_val = left.data.int_val + right.data.int_val};
                }
            }
        }else if(token == MINUS){
            get_token();
            numInfo right = term();
            if(left.type == FLOAT){
                if(right.type == INTEGER){
                    printf("warning float * int\n");
                    return (numInfo){.type=FLOAT , .data.float_val = left.data.float_val - (double)right.data.int_val};
                }else{
                    return (numInfo){.type=FLOAT , .data.float_val = left.data.float_val - right.data.float_val};
                }
            }
            if(left.type == INTEGER){
                if(right.type == FLOAT){
                    printf("warning int * float\n");
                    return (numInfo){.type=FLOAT , .data.float_val = (double)left.data.int_val - right.data.float_val};
                }else{
                    return (numInfo){.type=INTEGER , .data.int_val = left.data.int_val - right.data.int_val};
                }
            }
        }
    }
}

numInfo term(){
    numInfo left = factor();
    while(token == STAR || DIVISOR){
        if(token ==STAR){
            get_token();
            numInfo right = factor();
            if(left.type == FLOAT){
                if(right.type == INTEGER){
                    printf("warning float * int\n");
                    return (numInfo){.type=FLOAT , .data.float_val = left.data.float_val * (double)right.data.int_val};
                }else{
                    return (numInfo){.type=FLOAT , .data.float_val = left.data.float_val * right.data.float_val};
                }
            }
            if(left.type == INTEGER){
                if(right.type == FLOAT){
                    printf("warning int * float\n");
                    return (numInfo){.type=FLOAT , .data.float_val = (double)left.data.int_val * right.data.float_val};
                }else{
                    return (numInfo){.type=INTEGER , .data.int_val = left.data.int_val * right.data.int_val};
                }
            }
        }else if(token == DIVISOR){
            get_token();
            numInfo right = factor();
            if(left.type == FLOAT){
                if(right.type == INTEGER){
                    printf("warning float / int\n");
                    return (numInfo){.type=FLOAT , .data.float_val = left.data.float_val / (float)right.data.int_val};
                }else{
                    return (numInfo){.type=FLOAT , .data.float_val = left.data.float_val / right.data.float_val};
                }
            }
            if(left.type == INTEGER){
                if(right.type == FLOAT){
                    printf("warning int * float\n");
                    return (numInfo){.type=FLOAT , .data.float_val = (float)left.data.int_val / right.data.float_val};
                }else{
                    return (numInfo){.type=INTEGER , .data.int_val = left.data.int_val / right.data.int_val};
                }
            }
        }
    }
}

numInfo factor(){
    if(token == FLOAT){
        get_token();
        return (numInfo){.type = FLOAT , .data.float_val = atof(lexeme)};
    }else if(token == INTEGER){
        get_token();
        return (numInfo) {.type = INTEGER, .data.int_val = atoi(lexeme)};
    }else if(token == LPAREN){
        get_token();
        numInfo ret = expression();
        if(token == RPAREN){
            get_token();
            return ret;
        }
        else
            error= true;
    }else{
        error = true;
    }
}
int main(){
    getNonBlankChar();
    get_token();
    numInfo result = expression();
    if(token!=END){
        error = true;
    }
    if(error){
        printf("failed...\n");
    }
    if(result.type == INTEGER){
        printf("%d",result.data.int_val);
    }else{
        printf("%f",result.data.float_val);
    }
}