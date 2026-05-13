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
    int result;
} Equation;

typedef struct ExprNode {
    char token[MAX_EXPR_LENGTH];
    struct ExprNode *left;
    struct ExprNode *right;
} ExprNode;

static Equation equations[MAX_EQUATIONS];
static int equationCount = 0;

/* Clears the current list of loaded equations. */
void clearEquationList(void) {
    equationCount = 0;
}

/* Removes all spaces and tabs from an expression. */
void trimWhitespace(const char *src, char *dst) {
    int j = 0;
    for (int i = 0; src[i] != '\0'; i++) {
        if (!isspace((unsigned char)src[i])) {
            dst[j++] = src[i];
        }
    }
    dst[j] = '\0';
}

/* Returns true if the character is one of the four arithmetic operators. */
int isOperatorChar(char c) {
    return c == '+' || c == '-' || c == '*' || c == '/';
}

/* Returns true if the character is an opening bracket. */
int isOpeningBracket(char c) {
    return c == '(' || c == '[';
}

/* Returns true if the character is a closing bracket. */
int isClosingBracket(char c) {
    return c == ')' || c == ']';
}

/* Gives the priority of an operator. */
int precedence(char c) {
    if (c == '+' || c == '-') {
        return 1;
    }
    if (c == '*' || c == '/') {
        return 2;
    }
    return 0;
}

/* Returns true if + or - is being used as a sign before a number. */
int isUnarySign(const char *expr, int index, char prev) {
    if ((expr[index] == '+' || expr[index] == '-') && isdigit((unsigned char)expr[index + 1])) {
        if (prev == '\0' || isOperatorChar(prev) || isOpeningBracket(prev)) {
            return 1;
        }
    }
    return 0;
}

/* Copies part of a string into another string. */
void copySubstring(char *dst, const char *src, int length) {
    for (int i = 0; i < length; i++) {
        dst[i] = src[i];
    }
    dst[length] = '\0';
}

/* Reads one token from a postfix expression. */
int readPostfixToken(const char *postfix, int *index, char *token) {
    int length = 0;

    while (postfix[*index] == ' ') {
        (*index)++;
    }
    if (postfix[*index] == '\0') {
        return 0;
    }

    if ((postfix[*index] == '+' || postfix[*index] == '-') && isdigit((unsigned char)postfix[*index + 1])) {
        token[length++] = postfix[(*index)++];
        while (isdigit((unsigned char)postfix[*index])) {
            token[length++] = postfix[(*index)++];
        }
    } else if (isdigit((unsigned char)postfix[*index])) {
        while (isdigit((unsigned char)postfix[*index])) {
            token[length++] = postfix[(*index)++];
        }
    } else {
        token[length++] = postfix[(*index)++];
    }

    token[length] = '\0';
    return 1;
}

/* Copies the number that ends before a closing bracket. */
void getNumberBeforeBracket(const char *expr, int closeIndex, char *number) {
    int end = closeIndex - 1;
    int start = end;

    while (start >= 0 && isdigit((unsigned char)expr[start])) {
        start--;
    }
    copySubstring(number, expr + start + 1, end - start);
}

/* Copies the number that starts at the given index. */
void getNumberAt(const char *expr, int index, char *number) {
    int start = index;

    while (isdigit((unsigned char)expr[index])) {
        index++;
    }
    copySubstring(number, expr + start, index - start);
}

/* Checks if an infix expression is valid. */
int validateExpression(const char *expr, char *error) {
    char bracketStack[MAX_EXPR_LENGTH];
    int top = -1;
    char prev = '\0';

    if (expr[0] == '\0') {
        strcpy(error, "invalid: empty expression");
        return 0;
    }

    for (int i = 0; expr[i] != '\0'; i++) {
        char c = expr[i];

        if ((c == '+' || c == '-') && isdigit((unsigned char)expr[i + 1]) &&
            (prev == '\0' || isOpeningBracket(prev) ||
            (isOperatorChar(prev) && prev != c))) {
            i++;
            while (isdigit((unsigned char)expr[i + 1])) {
                i++;
            }
            prev = expr[i];
        } else if (isdigit((unsigned char)c)) {
            if (prev != '\0' && isClosingBracket(prev)) {
                char leftNumber[32];
                char rightNumber[32];
                getNumberBeforeBracket(expr, i - 1, leftNumber);
                getNumberAt(expr, i, rightNumber);
                sprintf(error, "invalid: there is no operator between %s%c%s", leftNumber, prev, rightNumber);
                return 0;
            }
            if (prev != '\0' && isdigit((unsigned char)prev)) {
                strcpy(error, "invalid expression");
                return 0;
            }
            while (isdigit((unsigned char)expr[i + 1])) {
                i++;
            }
            prev = expr[i];
        } else if (isOpeningBracket(c)) {
            if (prev != '\0' && (isdigit((unsigned char)prev) || isClosingBracket(prev))) {
                strcpy(error, "invalid expression");
                return 0;
            }
            bracketStack[++top] = c;
            prev = c;
        } else if (isClosingBracket(c)) {
            if (prev == '\0' || isOperatorChar(prev) || isOpeningBracket(prev)) {
                strcpy(error, "invalid expression");
                return 0;
            }
            if (top < 0) {
                strcpy(error, "invalid expression");
                return 0;
            }
            if (bracketStack[top] == '(' && c == ']' && top > 0 && bracketStack[top - 1] == '[') {
                strcpy(error, "invalid: ( is not closed");
                return 0;
            }
            if ((bracketStack[top] == '(' && c != ')') || (bracketStack[top] == '[' && c != ']')) {
                strcpy(error, "invalid expression");
                return 0;
            }
            top--;
            prev = c;
        } else if (isOperatorChar(c)) {
            if (prev == '\0' || isOperatorChar(prev) || isOpeningBracket(prev)) {
                strcpy(error, "invalid: operator is placed incorrectly");
                return 0;
            }
            prev = c;
        } else {
            sprintf(error, "invalid: unexpected character '%c'", c);
            return 0;
        }
    }

    if (isOperatorChar(prev)) {
        strcpy(error, "invalid expression");
        return 0;
    }
    if (top >= 0) {
        if (bracketStack[top] == '(') {
            strcpy(error, "invalid: ( is not closed");
        } else {
            strcpy(error, "invalid: [ is not closed");
        }
        return 0;
    }

    strcpy(error, "valid");
    return 1;
}

/* Converts a valid infix expression to postfix form. */
int infixToPostfix(const char *infix, char *postfix) {
    char stack[MAX_EXPR_LENGTH];
    int top = -1;
    int pos = 0;

    for (int i = 0; infix[i] != '\0'; i++) {
        char c = infix[i];
        char prev = (i == 0) ? '\0' : infix[i - 1];

        if (isUnarySign(infix, i, prev)) {
            int start = i;
            i++;
            while (isdigit((unsigned char)infix[i + 1])) {
                i++;
            }
            int length = i - start + 1;
            copySubstring(postfix + pos, infix + start, length);
            pos += length;
            postfix[pos++] = ' ';
            continue;
        }

        if (isdigit((unsigned char)c)) {
            int start = i;
            while (isdigit((unsigned char)infix[i + 1])) {
                i++;
            }
            int length = i - start + 1;
            copySubstring(postfix + pos, infix + start, length);
            pos += length;
            postfix[pos++] = ' ';
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
            while (top >= 0 && isOperatorChar(stack[top]) && precedence(stack[top]) >= precedence(c)) {
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

/* Evaluates a postfix expression using a stack. */
int evaluatePostfix(const char *postfix, int *result, char *error) {
    int stack[MAX_EXPR_LENGTH];
    int top = -1;
    char token[MAX_EXPR_LENGTH];
    int index = 0;

    while (readPostfixToken(postfix, &index, token)) {
        if (strlen(token) == 1 && isOperatorChar(token[0])) {
            if (top < 1) {
                strcpy(error, "invalid: malformed postfix expression");
                return 0;
            }
            int right = stack[top--];
            int left = stack[top--];
            switch (token[0]) {
                case '+': stack[++top] = left + right; break;
                case '-': stack[++top] = left - right; break;
                case '*': stack[++top] = left * right; break;
                case '/':
                    if (right == 0) {
                        strcpy(error, "invalid: division by zero");
                        return 0;
                    }
                    stack[++top] = left / right;
                    break;
            }
        } else {
            stack[++top] = atoi(token);
        }
    }

    if (top != 0) {
        strcpy(error, "invalid: malformed postfix expression");
        return 0;
    }
    *result = stack[top];
    strcpy(error, "valid");
    return 1;
}

/* Creates one node for the expression tree. */
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

/* Builds an expression tree from a postfix expression. */
ExprNode *buildExpressionTree(const char *postfix) {
    ExprNode *stack[MAX_EXPR_LENGTH];
    int top = -1;
    int index = 0;
    char token[MAX_EXPR_LENGTH];

    while (readPostfixToken(postfix, &index, token)) {
        ExprNode *node = createNode(token);
        if (strlen(token) == 1 && isOperatorChar(token[0])) {
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

/* Frees all nodes in an expression tree. */
void freeTree(ExprNode *root) {
    if (root == NULL) {
        return;
    }
    freeTree(root->left);
    freeTree(root->right);
    free(root);
}

/* Prints the tree using inorder traversal. */
void printInorder(ExprNode *root) {
    if (root == NULL) {
        return;
    }
    int isOp = (isOperatorChar(root->token[0]) && root->token[1] == '\0');
    if (isOp) {
        printf("(");
    }
    printInorder(root->left);
    printf("%s", root->token);
    printInorder(root->right);
    if (isOp) {
        printf(")");
    }
}

/* Helper function for preorder traversal. */
void printPreorderHelper(ExprNode *root, int *first) {
    if (root == NULL) {
        return;
    }
    if (!(*first)) {
        printf(" ");
    }
    printf("%s", root->token);
    *first = 0;
    printPreorderHelper(root->left, first);
    printPreorderHelper(root->right, first);
}

/* Prints the tree using preorder traversal. */
void printPreorder(ExprNode *root) {
    int first = 1;
    printPreorderHelper(root, &first);
}

/* Helper function for postorder traversal. */
void printPostorderHelper(ExprNode *root, int *first) {
    if (root == NULL) {
        return;
    }
    printPostorderHelper(root->left, first);
    printPostorderHelper(root->right, first);
    if (!(*first)) {
        printf(" ");
    }
    printf("%s", root->token);
    *first = 0;
}

/* Prints the tree using postorder traversal. */
void printPostorder(ExprNode *root) {
    int first = 1;
    printPostorderHelper(root, &first);
}

/* Reads equations from the given input file. */
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
        Equation *eq = &equations[equationCount];
        size_t len = strlen(buffer);
        if (len > 0 && buffer[len - 1] == '\n') {
            buffer[len - 1] = '\0';
            len--;
        }
        if (len == 0) {
            continue;
        }
        strcpy(eq->raw, buffer);
        trimWhitespace(buffer, eq->cleaned);
        if (eq->cleaned[0] == '\0') {
            continue;
        }
        eq->postfix[0] = '\0';
        eq->result = 0;
        if (validateExpression(eq->cleaned, eq->error)) {
            eq->valid = 1;
            infixToPostfix(eq->cleaned, eq->postfix);
            if (!evaluatePostfix(eq->postfix, &eq->result, eq->error)) {
                eq->valid = 0;
                eq->postfix[0] = '\0';
            }
        } else {
            eq->valid = 0;
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

/* Prints whether each loaded equation is valid or invalid. */
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

/* Prints postfix form for all valid equations. */
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

/* Prints evaluation results for all valid equations. */
void printEvaluationResults(void) {
    if (equationCount == 0) {
        printf("No equations loaded. Please choose option 1 first.\n");
        return;
    }
    for (int i = 0; i < equationCount; i++) {
        if (equations[i].valid) {
            printf("Equation No. %d -> %d\n", i + 1, equations[i].result);
        }
    }
}

/* Prints all invalid equations and their errors. */
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

/* Lets the user choose an equation and prints its tree traversals. */
void expressionTreeMenu(void) {
    if (equationCount == 0) {
        printf("No equations loaded. Please choose option 1 first.\n");
        return;
    }
    int number;
    printf("Enter equation number for tree generation: ");
    scanf("%d", &number);
    while (getchar() != '\n');
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

/* Writes equation status, postfix, and results to output.txt. */
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
            fprintf(file, "Result: %d\n", equations[i].result);
        } else {
            fprintf(file, "Status: invalid\n");
            fprintf(file, "Error: %s\n", equations[i].error);
        }
        fprintf(file, "\n");
    }
    fclose(file);
    printf("Report written to output.txt\n");
}

/* Displays the main menu. */
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

/* Runs the menu loop. */
int main(void) {
    clearEquationList();
    int choice = 0;

    while (1) {
        showMenu();
        scanf("%d", &choice);
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
                if (len > 0 && filename[len - 1] == '\n') {
                    filename[len - 1] = '\0';
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
