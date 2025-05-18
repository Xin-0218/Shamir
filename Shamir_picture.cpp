#include <iostream>
#include <vector>
#include<ctime>
using namespace std;
int n, k;

unsigned char** Matrix(int n, int m)
{
    unsigned char** M = new unsigned char* [n];
    for (int i = 0; i < n; i++)
    {
        M[i] = new unsigned char[m];
    }
    return M;
}

void DeleteMatrix(unsigned char** M, int n)
{
    for (int i = 0; i < n; i++)
    {
        delete[] M[i];
    }
    delete[] M;
}

unsigned char exp_table[256];
unsigned char log_table[256];
const int GF = 256;
const int PP = 301;

unsigned char add(unsigned char a, unsigned char b)
{
    return a ^ b;
}

unsigned char subtract(unsigned char a, unsigned char b)
{
    return a ^ b;
}

unsigned char multiply(unsigned char a, unsigned char b)
{
    if (a == 0 || b == 0)
    {
        return 0;
    }
    else
    {
        return exp_table[(log_table[a] + log_table[b]) % (GF - 1)];
    }
}

unsigned char divide(unsigned char a, unsigned char b)
{
    if (b == 0)
    {
        throw std::invalid_argument("Division by zero");
    }

    int diff = log_table[a] - log_table[b];
    if (diff < 0)
    {
        diff = diff + (GF - 1);
    }

    return exp_table[diff];
}

void exp_log_table()
{
    exp_table[0] = 1;
    unsigned char primitive_polynomial = 0b100011101;
    unsigned short prim_poly = 0b100011101;

    for (int i = 1; i < GF - 1; i++)
    {
        unsigned short temp = (unsigned short)(exp_table[i - 1]) << 1;
        if (temp & GF)  // GF = 256 = 1 << 8
        {
            temp ^= prim_poly;
        }
        exp_table[i] = (unsigned char)temp;
    }

    for (int i = 0; i < GF - 1; i++)
    {
        log_table[exp_table[i]] = (unsigned char)i;
    }
}

unsigned char x_table[256][256];
unsigned char y_table[256] = { 0 };
unsigned char secret[256] = { 0 };
unsigned char s_table[256] = { 0 };
unsigned char delta = 0;
unsigned char delta_table[256] = { 0 };
unsigned char det_table[256][256];

/*vector<vector<unsigned char>> x_table;
vector<unsigned char> y_table;
vector<unsigned char> secret;
vector<unsigned char> s_table;
unsigned char delta = 0;
vector<unsigned char> delta_table;
vector<vector<unsigned char>> det_table;*/

void rand_num()
{
    for (int i = 1; i < k; i++)
    {
        secret[i] = rand() % 255 + 1;
    }
}

void tables()
{
    for (int i = 0; i < k; i++)
    {
        for (int j = 0; j < k; j++)
        {
            if (j == 0)
            {
                x_table[i][j] = 1;
            }
            else
            {
                x_table[i][j] = multiply(x_table[i][j - 1], i + 1);
            }
            det_table[i][j] = x_table[i][j];
        }
    }
}

void functionY()
{
    for (int i = 0; i < n; i++)
    {
        unsigned char x = 1, y = secret[0];
        for (int j = 1; j < k; j++)
        {
            x = multiply(x, i + 1);
            y = add(y, multiply(secret[j], x));
        }
        y_table[i] = y;
    }
}

void decrypt_num()
{
    int Snum;
    cout << "S :\n";
    cin >> Snum;
    for (int i = 0; Snum > 0; i++)
    {
        s_table[i] = Snum % 10;
        Snum = Snum / 10;
    }
}

unsigned char Det(unsigned char** M, int k)
{
    if (k == 1)
    {
        return M[0][0];
    }
    if (k == 2)
    {
        return subtract(multiply(M[0][0], M[1][1]), multiply(M[0][1], M[1][0]));
    }

    unsigned char det = 0;
    unsigned char** New_M = Matrix(k - 1, k - 1);

    for (int a = 0; a < k; a++)
    {
        for (int i = 1; i < k; i++)
        {
            int col_set = 0;
            for (int j = 0; j < k; j++)
            {
                if (j == a)
                {
                    col_set++;
                    continue;
                }
                New_M[i - 1][j - col_set] = M[i][j];
            }
        }

        unsigned char partial_det = multiply(M[0][a], Det(New_M, k - 1));
        if (a % 2 == 0)
        {
            det = add(det, partial_det);
        }
        else
        {
            det = subtract(det, partial_det);
        }
    }

    DeleteMatrix(New_M, k - 1);
    return det;
}

void calculate_delta()
{
    unsigned char** matrix = Matrix(k, k);
    for (int i = 0; i < k; i++)
    {
        for (int j = 0; j < k; j++)
        {
            matrix[i][j] = det_table[s_table[i] - 1][j];
        }
    }

    delta = Det(matrix, k);

    int col = 0;
    unsigned char** temp_matrix = Matrix(k, k);
    for (int i = 0; i < k; i++)
    {
        for (int j = 0; j < k; j++)
        {
            if (j == col)
                temp_matrix[i][j] = y_table[s_table[i] - 1];
            else
                temp_matrix[i][j] = matrix[i][j];
        }
    }
    delta_table[col] = Det(temp_matrix, k);
    DeleteMatrix(temp_matrix, k);

    /*for (int col = 0; col < k; col++)
    {
        int** temp_matrix = Matrix(k, k);
        for (int i = 0; i < k; i++)
        {
            for (int j = 0; j < k; j++)
            {
                if (j == col)
                    temp_matrix[i][j] = y_table[s_table[i] - 1];  // 只替換該欄成 y 值
                else
                    temp_matrix[i][j] = matrix[i][j];
            }
        }
        delta_table[col] = Det(temp_matrix, k);
        DeleteMatrix(temp_matrix, k);
    }*/

    DeleteMatrix(matrix, k);
}

unsigned char matrix_g[256][257];
unsigned char solution[256];

vector<unsigned char> gaussian_solve(vector<vector<unsigned char>>& mat)
{
    vector<unsigned char> res(k);

    for (int i = 0; i < k; i++)
    {
        if (mat[i][i] == 0)
        {
            for (int j = i + 1; j < k; j++)
            {
                if (mat[j][i] != 0)
                {
                    swap(mat[i], mat[j]);
                    break;
                }
            }
        }

        unsigned char inv = divide(1, mat[i][i]);
        for (int j = i; j <= k; j++)
        {
            mat[i][j] = multiply(mat[i][j], inv);
        }
        for (int j = i + 1; j < k; j++)
        {
            if (mat[j][i] != 0)
            {
                unsigned char factor = mat[j][i];
                for (int l = i; l <= k; l++)
                {
                    mat[j][l] = subtract(mat[j][l], multiply(factor, mat[i][l]));
                }
            }
        }
    }

    for (int i = k - 1; i >= 0; i--)
    {
        res[i] = mat[i][k];
        for (int j = i + 1; j < k; j++)
        {
            res[i] = subtract(res[i], multiply(mat[i][j], res[j]));
        }
    }
    return res;
}

FILE* output(const vector<vector<unsigned char>>& imgData, int n, int dataSize, const unsigned char* header)
{
    for (int i = 1; i <= n; i++)
    {
        FILE* fp_w;
        char outputFile[100];
        sprintf_s(outputFile, sizeof(outputFile), "output_%d.bmp", i);

        if (fopen_s(&fp_w, outputFile, "wb") != 0)
        {
            printf("create %s error！\n", outputFile);
            continue;
        }

        fwrite(header, sizeof(unsigned char), 54, fp_w);
        fwrite(imgData[i].data(), sizeof(unsigned char), dataSize, fp_w);
        fclose(fp_w);
        printf("success create %s\n", outputFile);
    }

    FILE* fp_p;
    if (fopen_s(&fp_p, "行列式_Picture.bmp", "wb") != 0)
    {
        printf("create error！\n");
        return nullptr;
    }

    fwrite(header, sizeof(unsigned char), 54, fp_p);
    return fp_p;
}

void decrypt_with_determinant(FILE* fp_p, const vector<vector<unsigned char>>& imgData, int dataSize)
{
    vector<unsigned char> PictureDelta(dataSize);

    clock_t start, end;
    start = clock();
    for (int i = 0; i < dataSize; i++)
    {
        for (int j = 0; j < k; j++)
        {
            y_table[j] = imgData[s_table[j]][i];
        }
        calculate_delta();
        PictureDelta[i] = divide(delta_table[0], delta);
    }
    end = clock();
    double elapsed_time = double(end - start) / CLOCKS_PER_SEC;
    cout << "行列式 time: " << elapsed_time << endl;

    for (int i = 0; i < dataSize; i++)
    {
        putc(PictureDelta[i], fp_p);
    }
    printf("success create 行列式_Picture.bmp\n");

    int c = 1;
    for (int i = 0; i < dataSize; i++)
    {
        if (PictureDelta[i] != imgData[0][i])
        {
            c = 0;
            break;
        }
    }
    if (c == 0)
        cout << "解密失敗" << endl;
    else
        cout << "解密成功" << endl;
}

void decrypt_with_gaussian(FILE* fp_g, const vector<vector<unsigned char>>& imgData, int dataSize)
{
    vector<unsigned char> PictureGaussian(dataSize);
    vector<vector<unsigned char>> mat(k, vector<unsigned char>(k + 1));

    clock_t start_g, end_g;
    start_g = clock();
    for (int i = 0; i < dataSize; i++)
    {
        for (int j = 0; j < k; j++)
        {
            y_table[j] = imgData[s_table[j]][i];
        }

        for (int j = 0; j < k; j++)
        {
            for (int l = 0; l < k; l++)
            {
                mat[j][l] = det_table[s_table[j] - 1][l];
            }
            mat[j][k] = y_table[j];
        }

        auto solution = gaussian_solve(mat);
        PictureGaussian[i] = solution[0];
    }
    end_g = clock();
    double elapsed_time = double(end_g - start_g) / CLOCKS_PER_SEC;
    cout << "高斯 time: " << elapsed_time << endl;

    for (int i = 0; i < dataSize; i++)
    {
        putc(PictureGaussian[i], fp_g);
    }
    fclose(fp_g);
    printf("success create 高斯_Picture.bmp\n");

    int c = 1;
    for (int i = 0; i < dataSize; i++)
    {
        if (PictureGaussian[i] != imgData[0][i])
        {
            c = 0;
            break;
        }
    }
    if (c == 0)
        cout << "解密失敗" << endl;
    else
        cout << "解密成功" << endl;
}

void decrypt_with_lagrange(FILE* fp_l, const vector<vector<unsigned char>>& imgData, int dataSize)
{
    vector<unsigned char> PictureLagrange(dataSize);

    clock_t start_l, end_l;
    start_l = clock();
    for (int i = 0; i < dataSize; i++)
    {
        for (int j = 0; j < k; j++)
        {
            y_table[j] = imgData[s_table[j]][i];
        }

        unsigned char secret = 0;
        for (int a = 0; a < k; a++)
        {
            unsigned char numerator = 1;
            unsigned char denominator = 1;
            for (int b = 0; b < k; b++)
            {
                if (a == b)
                {
                    continue;
                }
                numerator = multiply(numerator, s_table[b]);
                unsigned char x = subtract(s_table[a], s_table[b]);
                denominator = multiply(denominator, x);
            }
            unsigned char lagrange_coeff = divide(numerator, denominator);
            unsigned char term = multiply(y_table[a], lagrange_coeff);
            secret = add(secret, term);
        }
        PictureLagrange[i] = secret;
    }
    end_l = clock();
    double elapsed_time = double(end_l - start_l) / CLOCKS_PER_SEC;
    cout << "拉格朗日 time: " << elapsed_time << endl;

    for (int i = 0; i < dataSize; i++)
    {
        putc(PictureLagrange[i], fp_l);
    }
    fclose(fp_l);
    printf("success create 拉格朗日_Picture.bmp\n");

    int c = 1;
    for (int i = 0; i < dataSize; i++)
    {
        if (PictureLagrange[i] != imgData[0][i])
        {
            c = 0;
            break;
        }
    }
    if (c == 0)
        cout << "解密失敗" << endl;
    else
        cout << "解密成功" << endl;
}

int main()
{
    exp_log_table();

    cout << "n: "; cin >> n;
    cout << "k: "; cin >> k;

    /*x_table.resize(k, vector<unsigned char>(k));
    y_table.resize(n);
    secret.resize(k);
    s_table.resize(n);
    delta_table.resize(k);
    det_table.resize(k, vector<unsigned char>(k));*/

    FILE* fp_n;
    if (fopen_s(&fp_n, "210614.bmp", "rb") != 0)
    {
        printf("open error！\n");
        return 0;
    }

    cout << endl << "檔案大小: ";

    unsigned char c;
    for (int i = 0; i < 18; i++)
    {
        c = getc(fp_n);
    }

    int w = 0;
    w += getc(fp_n);
    w += getc(fp_n) * 256;
    w += getc(fp_n) * 256 * 256;
    w += getc(fp_n) * 256 * 256 * 256;
    cout << w << "*";

    int h = 0;
    h += getc(fp_n);
    h += getc(fp_n) * 256;
    h += getc(fp_n) * 256 * 256;
    h += getc(fp_n) * 256 * 256 * 256;
    cout << h << endl;

    cout << endl;

    int rowSize = 0;
    int dataSize;
    int new_w = w * 3;
    if (new_w % 4 != 0)
    {
        rowSize = new_w + (4 - new_w % 4);
    }
    else
    {
        rowSize = w * 3;
    }
    //cout << rowSize << endl;
    dataSize = rowSize * h;

    FILE* fp_r;
    if (fopen_s(&fp_r, "210614.bmp", "rb") != 0)
    {
        printf("open error！\n");
        return 0;
    }

    unsigned char header[54];
    fread(header, sizeof(unsigned char), 54, fp_r);

    vector<vector<unsigned char>> imgData(n + 1, vector<unsigned char>(dataSize));
    for (int i = 0; i < dataSize; i++)
    {
        imgData[0][i] = getc(fp_r);
        if (imgData[0][i] == NULL)
        {
            imgData[0][i] = '00';
        }
    }
    fclose(fp_r);

    tables();

    for (int i = 0; i < dataSize; i++)
    {
        secret[0] = imgData[0][i];
        rand_num();
        functionY();
        for (int j = 1; j <= n; j++)
        {
            imgData[j][i] = y_table[j - 1];
        }
    }

    FILE* fp_p = output(imgData, n, dataSize, header);

    cout << endl;
    decrypt_num();
    cout << endl;

    if (fp_p != nullptr)
    {
        decrypt_with_determinant(fp_p, imgData, dataSize);
        cout << endl;

        FILE* fp_g;
        if (fopen_s(&fp_g, "高斯_Picture.bmp", "wb") == 0)
        {
            fwrite(header, sizeof(unsigned char), 54, fp_g);
            decrypt_with_gaussian(fp_g, imgData, dataSize);
        }
        else
        {
            printf("create 高斯_Picture.bmp error！\n");
        }

        cout << endl;

        FILE* fp_l;
        if (fopen_s(&fp_l, "拉格朗日_Picture.bmp", "wb") == 0)
        {
            fwrite(header, sizeof(unsigned char), 54, fp_l);
            decrypt_with_lagrange(fp_l, imgData, dataSize);
        }
        else
        {
            printf("create 拉格朗日_Picture.bmp error！\n");
        }
    }
    return 0;
}
