/*
 * P1_1220022_emam_samara_1.c
 * Student Name: emam samara
 * Student ID: 1220022
 * Section No.: 1
 *
 * Calculator System Project
 * This program reads a list of infix equations from an input file,
 * validates each equation, converts valid equations to postfix notation,
 * evaluates postfix expressions, prints invalid equations, allows expression
 * tree traversal, and writes a summary report to output.txt.
 */

#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_EXPR_LENGTH 1024
#define MAX_EQUATIONS 200
#define MAX_POSTFIX_LENGTH 2048

typedef struct Equation {
    char raw[MAX_EXPR_LENGTH];
    char cleaned[MAX_EXPR_LENGTH];
    char postfix[MAX_POSTFIX_LENGTH];
    int valid;
    char error[128];
    long long result;
} Equation;

typedef struct ExprNode {
    char token[MAX_EXPR_LENGTH];
    struct ExprNode *left;
    struct ExprNode *right;
} ExprNode;

static Equation equations[MAX_EQUATIONS];
static int equationCount = 0;

void clearEquationList(void) {
    equationCount = 0;
    for (int i = 0; i < MAX_EQUATIONS; i++) {
        equations[i].raw[0] = '\0';
        equations[i].cleaned[0] = '\0';
        equations[i].postfix[0] = '\0';
        equations[i].valid = 0;
        equations[i].error[0] = '\0';
        equations[i].result = 0;
    }
}

void trimWhitespace(const char *src, char *dst) {
    int j = 0;
    for (int i = 0; src[i] != '\0'; i++) {
        if (!isspace((unsigned char)src[i])) {
            dst[j++] = src[i];
        }
    }
    dst[j] = '\0';
}

int isOperatorChar(char c) {
    return c == '+' || c == '-' || c == '*' || c == '/';
}

int isOpeningBracket(char c) {
    return c == '(' || c == '[';
}

int isClosingBracket(char c) {
    return c == ')' || c == ']';
}

int bracketsMatch(char open, char close) {
    return (open == '(' && close == ')') || (open == '[' && close == ']');
}

int precedence(char c) {
    if (c == '+' || c == '-') {
        return 1;
    }
    if (c == '*' || c == '/') {
        return 2;
    }
    return 0;
}

void copyToken(const char *src, int length, char *dst) {
    for (int i = 0; i < length; i++) {
        dst[i] = src[i];
    }
    dst[length] = '\0';
}

int validateExpression(const char *expr, char *error) {
    char bracketStack[MAX_EXPR_LENGTH];
    int bracketTop = -1;
    char lastType = 0; // 'n' number, 'o' operator, '(' left paren, ')' right paren
    int expectOperand = 1;
    char lastNumber[32] = "";

    for (int i = 0; expr[i] != '\0'; i++) {
        char c = expr[i];

        if (isdigit((unsigned char)c)) {
            if (lastType == 'n' || lastType == ')') {
                sprintf(error, "invalid: there is no operator between %s and %c", lastNumber, c);
                return 0;
            }
            lastType = 'n';
            expectOperand = 0;
            int start = i;
            while (isdigit((unsigned char)expr[i])) {
                i++;
            }
            int len = i - start;
            if ((size_t)len < sizeof(lastNumber)) {
                copyToken(expr + start, len, lastNumber);
            }
            i--;
            continue;
        }

        if (isOpeningBracket(c)) {
            if (lastType == 'n' || lastType == ')') {
                sprintf(error, "invalid: there is no operator before '%c'", c);
                return 0;
            }
            bracketStack[++bracketTop] = c;
            lastType = '(';
            expectOperand = 1;
            continue;
        }

        if (isClosingBracket(c)) {
            if (lastType == 'o' || lastType == '(' || lastType == 0) {
                sprintf(error, "invalid: %c is not opened or empty parentheses/brackets", c);
                return 0;
            }
            if (bracketTop < 0) {
                sprintf(error, "invalid: %c is not opened", c);
                return 0;
            }
            if (!bracketsMatch(bracketStack[bracketTop], c)) {
                sprintf(error, "invalid: '%c' closes '%c' incorrectly", c, bracketStack[bracketTop]);
                return 0;
            }
            bracketTop--;
            lastType = ')';
            expectOperand = 0;
            continue;
        }

        if (isOperatorChar(c)) {
            if (expectOperand) {
                if (c == '+' || c == '-') {
                    if (expr[i + 1] != '\0' && isdigit((unsigned char)expr[i + 1])) {
                        int j = i + 1;
                        while (isdigit((unsigned char)expr[j])) {
                            j++;
                        }
                        lastType = 'n';
                        expectOperand = 0;
                        int len = j - i;
                        if ((size_t)len < sizeof(lastNumber)) {
                            copyToken(expr + i, len, lastNumber);
                        }
                        i = j - 1;
                        continue;
                    }
                }
                sprintf(error, "invalid: operator '%c' is placed incorrectly", c);
                return 0;
            }
            lastType = 'o';
            expectOperand = 1;
            continue;
        }

        sprintf(error, "invalid: unexpected character '%c'", c);
        return 0;
    }

    if (expectOperand) {
        sprintf(error, "invalid: expression ends with an operator");
        return 0;
    }
    if (bracketTop >= 0) {
        sprintf(error, "invalid: %c is not closed", bracketStack[bracketTop]);
        return 0;
    }
    strcpy(error, "valid");
    return 1;
}

int infixToPostfix(const char *infix, char *postfix) {
    char stack[MAX_EXPR_LENGTH];
    int top = -1;
    int pos = 0;

    for (int i = 0; infix[i] != '\0'; i++) {
        char c = infix[i];

        if (isdigit((unsigned char)c) || ((c == '+' || c == '-') && (i == 0 || infix[i - 1] == '(' || isOperatorChar(infix[i - 1])) && isdigit((unsigned char)infix[i + 1]))) {
            int start = i;
            if ((c == '+' || c == '-') && isdigit((unsigned char)infix[i + 1])) {
                i++;
                while (isdigit((unsigned char)infix[i])) {
                    i++;
                }
            } else {
                i++;
                while (isdigit((unsigned char)infix[i])) {
                    i++;
                }
            }
            int length = i - start;
            copyToken(infix + start, length, postfix + pos);
            pos += length;
            postfix[pos++] = ' ';
            i--;
            continue;
        }

        if (c == '(' || c == '[') {
            stack[++top] = c;
            continue;
        }
        if (c == ')' || c == ']') {
            char open = (c == ')') ? '(' : '[';
            while (top >= 0 && stack[top] != open) {
                postfix[pos++] = stack[top--];
                postfix[pos++] = ' ';
            }
            if (top >= 0) {
                top--;
            }
            continue;
        }

        if (isOperatorChar(c)) {
            while (top >= 0 && precedence(stack[top]) >= precedence(c)) {
                postfix[pos++] = stack[top--];
                postfix[pos++] = ' ';
            }
            stack[++top] = c;
            continue;
        }
    }

    while (top >= 0) {
        postfix[pos++] = stack[top--];
        postfix[pos++] = ' ';
    }

    if (pos > 0 && postfix[pos - 1] == ' ') {
        pos--;
    }
    postfix[pos] = '\0';
    return 1;
}

int evaluatePostfix(const char *postfix, long long *result, char *error) {
    long long stack[MAX_EXPR_LENGTH];
    int top = -1;
    char token[MAX_EXPR_LENGTH];
    int i = 0;

    while (postfix[i] != '\0') {
        if (postfix[i] == ' ') {
            i++;
            continue;
        }

        int length = 0;
        if (isOperatorChar(postfix[i]) && (postfix[i + 1] == ' ' || postfix[i + 1] == '\0')) {
            token[length++] = postfix[i++];
        } else {
            if (postfix[i] == '+' || postfix[i] == '-') {
                token[length++] = postfix[i++];
            }
            while (isdigit((unsigned char)postfix[i])) {
                token[length++] = postfix[i++];
            }
        }
        token[length] = '\0';

        if (length == 1 && isOperatorChar(token[0])) {
            if (top < 1) {
                strcpy(error, "invalid: malformed postfix expression");
                return 0;
            }
            long long right = stack[top--];
            long long left = stack[top--];
            long long value = 0;
            switch (token[0]) {
                case '+': value = left + right; break;
                case '-': value = left - right; break;
                case '*': value = left * right; break;
                case '/':
                    if (right == 0) {
                        strcpy(error, "invalid: division by zero");
                        return 0;
                    }
                    value = left / right;
                    break;
                default: value = 0; break;
            }
            stack[++top] = value;
        } else {
            stack[++top] = atoll(token);
        }
    }

    if (top == 0) {
        *result = stack[top];
        strcpy(error, "valid");
        return 1;
    }
    strcpy(error, "invalid: malformed postfix expression");
    return 0;
}

ExprNode *createNode(const char *token) {
    ExprNode *node = malloc(sizeof(ExprNode));
    if (node == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(EXIT_FAILURE);
    }
    strcpy(node->token, token);
    node->left = NULL;
    node->right = NULL;
    return node;
}

ExprNode *buildExpressionTree(const char *postfix) {
    ExprNode *stack[MAX_EXPR_LENGTH];
    int top = -1;
    int i = 0;
    while (postfix[i] != '\0') {
        if (postfix[i] == ' ') {
            i++;
            continue;
        }

        char token[MAX_EXPR_LENGTH];
        int len = 0;
        if (isOperatorChar(postfix[i]) && (postfix[i + 1] == ' ' || postfix[i + 1] == '\0')) {
            token[len++] = postfix[i++];
        } else {
            if (postfix[i] == '+' || postfix[i] == '-') {
                token[len++] = postfix[i++];
            }
            while (isdigit((unsigned char)postfix[i])) {
                token[len++] = postfix[i++];
            }
        }
        token[len] = '\0';

        ExprNode *node = createNode(token);
        if (len == 1 && isOperatorChar(token[0])) {
            if (top < 1) {
                free(node);
                return NULL;
            }
            node->right = stack[top--];
            node->left = stack[top--];
        }
        stack[++top] = node;
    }

    if (top == 0) {
        return stack[top];
    }
    return NULL;
}

void freeTree(ExprNode *root) {
    if (root == NULL) {
        return;
    }
    freeTree(root->left);
    freeTree(root->right);
    free(root);
}

void printInorder(ExprNode *root) {
    if (root == NULL) {
        return;
    }
    if (isOperatorChar(root->token[0]) && root->token[1] == '\0') {
        printf("(");
    }
    printInorder(root->left);
    printf("%s", root->token);
    printInorder(root->right);
    if (isOperatorChar(root->token[0]) && root->token[1] == '\0') {
        printf(")");
    }
}

void printPreorder(ExprNode *root) {
    if (root == NULL) {
        return;
    }
    printf("%s", root->token);
    if (root->left != NULL) {
        printf(" ");
        printPreorder(root->left);
    }
    if (root->right != NULL) {
        printf(" ");
        printPreorder(root->right);
    }
}

void printPostorder(ExprNode *root) {
    if (root == NULL) {
        return;
    }
    printPostorder(root->left);
    if (root->left != NULL) {
        printf(" ");
    }
    printPostorder(root->right);
    if (root->right != NULL) {
        printf(" ");
    }
    printf("%s", root->token);
}

void loadEquationsFromFile(const char *filename) {
    if (filename[0] == '\0') {
        printf("File name cannot be empty.\n");
        return;
    }

    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        printf("Unable to open file '%s'. Please check the name and try again.\n", filename);
        return;
    }

    clearEquationList();
    char buffer[MAX_EXPR_LENGTH];
    while (fgets(buffer, MAX_EXPR_LENGTH, file) != NULL && equationCount < MAX_EQUATIONS) {
        size_t len = strlen(buffer);
        while (len > 0 && (buffer[len - 1] == '\n' || buffer[len - 1] == '\r')) {
            buffer[--len] = '\0';
        }
        if (len == 0) {
            continue;
        }
        strcpy(equations[equationCount].raw, buffer);
        trimWhitespace(buffer, equations[equationCount].cleaned);
        if (equations[equationCount].cleaned[0] == '\0') {
            continue;
        }
        if (validateExpression(equations[equationCount].cleaned, equations[equationCount].error)) {
            equations[equationCount].valid = 1;
            infixToPostfix(equations[equationCount].cleaned, equations[equationCount].postfix);
            if (!evaluatePostfix(equations[equationCount].postfix, &equations[equationCount].result, equations[equationCount].error)) {
                equations[equationCount].valid = 0;
            }
        } else {
            equations[equationCount].valid = 0;
            equations[equationCount].postfix[0] = '\0';
            equations[equationCount].result = 0;
        }
        equationCount++;
    }
    fclose(file);
    if (equationCount == 0) {
        printf("No equations were loaded from '%s'.\n", filename);
    } else {
        printf("Loaded %d equations from '%s'.\n", equationCount, filename);
    }
}

void printValidityReport(void) {
    if (equationCount == 0) {
        printf("No equations loaded. Please choose option 1 first.\n");
        return;
    }
    for (int i = 0; i < equationCount; i++) {
        printf("Equation No. %d -> ", i + 1);
        if (equations[i].valid) {
            printf("valid\n");
        } else {
            printf("%s\n", equations[i].error);
        }
    }
}

void printPostfixExpressions(void) {
    if (equationCount == 0) {
        printf("No equations loaded. Please choose option 1 first.\n");
        return;
    }
    for (int i = 0; i < equationCount; i++) {
        if (equations[i].valid) {
            printf("Equation No. %d postfix: %s\n", i + 1, equations[i].postfix);
        }
    }
}

void printEvaluationResults(void) {
    if (equationCount == 0) {
        printf("No equations loaded. Please choose option 1 first.\n");
        return;
    }
    for (int i = 0; i < equationCount; i++) {
        if (equations[i].valid) {
            printf("Equation No. %d -> %lld\n", i + 1, equations[i].result);
        }
    }
}

void printInvalidEquations(void) {
    if (equationCount == 0) {
        printf("No equations loaded. Please choose option 1 first.\n");
        return;
    }
    int count = 0;
    for (int i = 0; i < equationCount; i++) {
        if (!equations[i].valid) {
            printf("Equation No. %d: %s [%s]\n", i + 1, equations[i].raw, equations[i].error);
            count++;
        }
    }
    if (count == 0) {
        printf("No invalid equations found.\n");
    }
}

void expressionTreeMenu(void) {
    if (equationCount == 0) {
        printf("No equations loaded. Please choose option 1 first.\n");
        return;
    }
    int number;
    printf("Enter equation number for tree generation: ");
    if (scanf("%d", &number) != 1) {
        while (getchar() != '\n');
        printf("Invalid input. Please enter a number.\n");
        return;
    }
    if (number < 1 || number > equationCount) {
        printf("Equation number must be between 1 and %d.\n", equationCount);
        return;
    }
    int index = number - 1;
    if (!equations[index].valid) {
        printf("Equation No. %d is invalid and cannot build an expression tree.\n", number);
        return;
    }
    ExprNode *root = buildExpressionTree(equations[index].postfix);
    if (root == NULL) {
        printf("Failed to build expression tree for Equation No. %d.\n", number);
        return;
    }

    printf("Inorder (infix): ");
    printInorder(root);
    printf("\n");

    printf("Postorder (postfix): ");
    printPostorder(root);
    printf("\n");

    printf("Preorder (prefix): ");
    printPreorder(root);
    printf("\n");

    freeTree(root);
}

void writeReportToFile(void) {
    if (equationCount == 0) {
        printf("No equations loaded. Please choose option 1 first.\n");
        return;
    }
    FILE *file = fopen("output.txt", "w");
    if (file == NULL) {
        printf("Unable to open output.txt for writing.\n");
        return;
    }

    for (int i = 0; i < equationCount; i++) {
        fprintf(file, "Equation No. %d: %s\n", i + 1, equations[i].raw);
        if (equations[i].valid) {
            fprintf(file, "Status: valid\n");
            fprintf(file, "Postfix: %s\n", equations[i].postfix);
            fprintf(file, "Result: %lld\n", equations[i].result);
        } else {
            fprintf(file, "Status: invalid\n");
            fprintf(file, "Error: %s\n", equations[i].error);
        }
        fprintf(file, "\n");
    }
    fclose(file);
    printf("Report written to output.txt\n");
}

void showMenu(void) {
    printf("\nCalculator System Menu:\n");
    printf("1. Read equations from file\n");
    printf("2. Check validity of equations\n");
    printf("3. Convert valid equations to postfix\n");
    printf("4. Evaluate valid equations\n");
    printf("5. Print invalid equations\n");
    printf("6. Expression tree for a valid equation\n");
    printf("7. Save all equation results to output.txt\n");
    printf("8. Exit\n");
    printf("Choose an option: ");
}

int main(void) {
    clearEquationList();
    int choice = 0;

    while (1) {
        showMenu();
        if (scanf("%d", &choice) != 1) {
            while (getchar() != '\n');
            printf("Invalid input. Please enter a number between 1 and 8.\n");
            continue;
        }
        while (getchar() != '\n');

        switch (choice) {
            case 1: {
                char filename[256];
                printf("Enter input file name: ");
                if (fgets(filename, sizeof(filename), stdin) == NULL) {
                    printf("Unable to read file name.\n");
                    break;
                }
                size_t len = strlen(filename);
                while (len > 0 && (filename[len - 1] == '\n' || filename[len - 1] == '\r')) {
                    filename[--len] = '\0';
                }
                loadEquationsFromFile(filename);
                break;
            }
            case 2:
                printValidityReport();
                break;
            case 3:
                printPostfixExpressions();
                break;
            case 4:
                printEvaluationResults();
                break;
            case 5:
                printInvalidEquations();
                break;
            case 6:
                expressionTreeMenu();
                break;
            case 7:
                writeReportToFile();
                break;
            case 8:
                printf("Exiting the calculator system.\n");
                return 0;
            default:
                printf("Please select a valid option between 1 and 8.\n");
                break;
        }
    }

    return 0;
}
