#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#define MAX_LEN 256 

typedef enum { PLUS , MINUS, STAR , DIVISOR , LPAREN , RPAREN , INT ,FLOAT_NUM, ENDTOKEN} Token;
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
char lexeme[MAX_LEN];
int idx = 0; //lexeme index 가리킴
bool error = false;  // 입력값 읽으면서 잘못된 입력이 있는지 체크

void clearLexeme();
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

//문자열에 읽은 문자를 저장한다
void addChar(){
    lexeme[idx++] = nextChar;
}

//읽은 문자가 어떤 토큰인지 알려주는 메서드 
void get_token(){
    clearLexeme();
    while(nextChar == ' '){
        nextChar = getchar();
    }
    if(nextChar >= '0' && nextChar <= '9'){
        addChar();
        nextChar = getchar();
        while(nextChar>= '0' && nextChar <= '9'){
            addChar();
            nextChar = getchar();
        }
        if(nextChar == '.'){
            addChar();
            nextChar = getchar();
            if(nextChar >='0' && nextChar <= '9'){
                while (nextChar >= '0' && nextChar <= '9')
                {
                    addChar();
                    nextChar = getchar();
                }
                token=FLOAT_NUM;
            }else{   // . 으로 끝나서 소수점 숫자 없으면 에러
                token=ENDTOKEN;
                error=true;
            }
            
        }else{
            token = INT;
        }
    }else if(nextChar == '+'){
        token=PLUS;
        addChar();
        nextChar = getchar();
    }else if(nextChar == '-'){
        token = MINUS;
        addChar();
        nextChar = getchar();
    }else if(nextChar == '*'){
        token = STAR;
        addChar();
        nextChar = getchar();
    }else if(nextChar == '/'){
        token = DIVISOR;
        addChar();
        nextChar = getchar();
    }else if(nextChar == '('){
        token = LPAREN;
        addChar();
        nextChar = getchar();
    }else if(nextChar == ')'){
        token = RPAREN;
        addChar();
        nextChar = getchar();
    }else if(nextChar == EOF || nextChar =='\n'){
        token = ENDTOKEN;
    }else{
        token = ENDTOKEN;
        error=true;
    }

}

//expression -> term (+|- term)
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

//term -> factor (*|/ factor)
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
            printf("failed...\n");
            exit(1);
        }
    }else{
        error = true;
        printf("failed...\n");
        exit(1);
    }
}
int main(){
    nextChar = getchar();
    get_token();
    numInfo result = expression();
    if(token!=ENDTOKEN){
        error = true;
    }
    if(error){
        printf("failed...\n");
        exit(1);
    }
    if(result.type == INTEGER){
        printf("%d\n",result.data.int_val);
    }else{
        printf("%f\n",result.data.float_val);
    }
    return 0;
}