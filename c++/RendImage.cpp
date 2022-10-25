#include <iostream>
#include <string>
#include <cmath>
#include <cstdio>
#include <fstream>
#include <cstdint>
#include <omp.h>
//#include <chrono>
#include <ctime>
using namespace std;

int main(int argc, char* argv[]) {
	double TimeStart;
	double TimeFinish;
	char* fileInputName;
	char* fileOutputName;
	float ratio; // ���������� ����������� �������������
	int countThreads = 0;
	int width = 0;
	int height = 0;
	int maxBright = -1;
	int partMaxBW; //���-�� �������� ������� ����� ������������
	int countPixels = 0; //���-�� �������� width * height
	unsigned char *outReds = new unsigned char[1]; //������� ��� ������ ��� ���������� ������
	unsigned char *outGreens = new unsigned char[1];
	unsigned char *outBlues = new unsigned char[1];
	unsigned char *outGreys = new unsigned char[1];

	if (argc != 5) {
		cout << "You must enter your input in format: <countThreads> <fileInputName> <fileOutputName> <Ratio>" << endl;
		cout << "FileInput must be P6(PPM) or P5(PGM)" << endl;
		cout << "Ratio must be a float in range[0.0; 0.5)" << endl;
		cout << "countThreads must be an integer >= 0" << endl;
		return 1;
	}

	string str1 = argv[1];
	string str4 = argv[4];

	try { //���-�� �������
		countThreads = stoi(argv[1]);
		if (countThreads < 0) {
			cout << "countThreads must be >= 0";
			return 1;
		}
	}
	catch (invalid_argument e) {
		cout << "countThreads isn't integer";
		return 1;
	}

	if (countThreads != 0) { //������ ���-�� ������� ����� �� ��������
		omp_set_num_threads(countThreads);
	}

	try { //����������
		ratio = stof(argv[4]);
		if (ratio >= 0.5 || ratio < 0) {
			cout << "Ratio must be in range[0.0, 0.5)";
			return 1;
		}
	}
	catch (invalid_argument e) {
		cout << "Ratio isn't float";
		return 1;
	}
	fileInputName = argv[2];
	fileOutputName = argv[3];
	ifstream inImage;
	inImage.open(fileInputName, ios::binary);
	if (!inImage.is_open()) {
		cout << "File: " + string(fileInputName) + " cannot read and/or not found";
		return 1;
	}

	string typeFile;
	try { // ����� ���������� ������ � ��������� ������
		getline(inImage, typeFile);
		if (typeFile != "P5" && typeFile != "P6") { //���������� �����
			cout << "FileInput isn't P6 and isn't P5 format PNM" << endl;
			inImage.close();
			return 1;
		}
		string strBuf = "";
		char ch;

		while (!inImage.eof()) { // width
			inImage.get(ch);
			if (!isdigit(ch)) {
				break;
			}
			else {
				strBuf += ch;
			}
		}
		width = stoi(strBuf);

		strBuf = "";
		while (!inImage.eof()) { // height
			inImage.get(ch);
			if (!isdigit(ch)) {
				break;
			}
			else {
				strBuf += ch;
			}
		}
		height = stoi(strBuf);

		if (height <= 0 || width <= 0) {
			cout << "FileInput uncorrectly, width and height image must be > 0";
			return 1;
		}
		countPixels = width * height; // ����� ���-�� ��������
		partMaxBW = int(ratio * width * height); // ������ ���-�� �������� ������� ����� ������������

		strBuf = "";
		while (!inImage.eof()) { // maxBright
			inImage.get(ch);
			if (!isdigit(ch)) {
				break;
			}
			else {
				strBuf += ch;
			}
		}
		maxBright = stoi(strBuf);

		if (maxBright != 255) {
			cout << "FileInput uncorrectly, maximum bright must be 255";
			return 1;
		}

		if (typeFile == "P5") { // ������������ �� pgm � ppm
//pgm
			unsigned char *greys = new unsigned char[countPixels]; //��� ����������� �������
			
			for (int i = 0; i < countPixels; i++) {
				inImage.get(ch);
				greys[i] = ch;
			}
			inImage.close();
			TimeStart = clock();

			int* sortGrey = new int[256]; // ������ ��� ���������� �� 256 ��������� �������
			for (int i = 0; i < 256; i++) {
				sortGrey[i] = 0;
			}

			for (int i = 0; i < countPixels; i++) {// �������
				sortGrey[greys[i]]++;
			}

			int maxBGrey = 0, maxWGrey = 0; //������� � �������� ��������������

			int minPix = 0;
			int maxPix = 255;
			int sum = 0;
			#pragma omp parallel sections private(sum) shared(partMaxBW, sortGrey, minPix, maxPix, maxBGrey, maxWGrey)
			{	//����� k-�� ���������� ����������� ������ � ����� �� ������ �������� ����
				#pragma omp section
				{
					sum = 0;
					while (sum < partMaxBW && minPix < 256) {
						sum += sortGrey[minPix];
						minPix++;
					}
					maxBGrey = max(0, minPix - 1);
				}
				#pragma omp section
				{
					sum = 0;
					while (sum < partMaxBW && maxPix >= 0) {
						sum += sortGrey[maxPix];
						maxPix--;
					}
					maxWGrey = min(255, maxPix + 1);
				}
			}
			if (maxWGrey == maxBGrey) { 
				#pragma omp parallel for schedule(static) default(none) shared(greys, countPixels, maxBGrey, outGreys)
				for (int i = 0; i < countPixels; i++) {
					if (greys[i] <= maxBGrey) {
						outGreys[i] = 0;
					}
					else {
						outGreys[i] = 255;
					}
				}
			}
			else {
				float gr = 255.0 / (maxWGrey - maxBGrey);
				float grd = gr * maxBGrey;
				outGreys = new unsigned char[countPixels];
				#pragma omp parallel for schedule(static) default(none) shared(gr, grd, sortGrey, greys)
				for (int i = 0; i < 256; i++) { //��������� ������� ���� ������� (�� ��� ��������� ��������)
					sortGrey[i] = min(255, max(0, int(round(i * gr - grd))));
				}
				#pragma omp parallel for schedule(static) default(none) shared(gr, grd, greys, countPixels, sortGrey, outGreys)
				for (int i = 0; i < countPixels; i++) { //��������� ������ �������
					outGreys[i] = sortGrey[greys[i]];
				}
			}
			TimeFinish = clock();
			delete[] sortGrey;
			delete[] greys;
		}
		else {
//ppm
			unsigned char *reds = new unsigned char[countPixels]; //������� ���� ��������� �������� �� 3 �������� �������
			unsigned char *greens = new unsigned char[countPixels];
			unsigned char *blues = new unsigned char[countPixels];;
			for (int i = 0; i < countPixels * 3; i++) {
				inImage.get(ch);
				if (i % 3 == 0) {
					reds[i / 3] = ch;
				}
				else if (i % 3 == 1) {
					greens[i / 3] = ch;
				}
				else {
					blues[i / 3] = ch;
				}
			}
			inImage.close();
//���� ������� �������� ������� ����� ������������
			TimeStart = clock();
			int *sortRed = new int[256]; //������� ��� ���������� �� 256 ��������� �� ������� �����
			int *sortGreen = new int[256];
			int *sortBlue = new int[256];
			for (int i = 0; i < 256; i++) {
				sortRed[i] = 0;
				sortGreen[i] = 0;
				sortBlue[i] = 0;
			}
#pragma omp parallel sections
			{
				#pragma omp section
				{
					for (int i = 0; i < countPixels; i++) { //�������
						sortRed[reds[i]]++;
					}
				}
				#pragma omp section
				{
					for (int i = 0; i < countPixels; i++) { //�������
						sortGreen[greens[i]]++;
					}
				}
				#pragma omp section
				{
					for (int i = 0; i < countPixels; i++) { //�������
						sortBlue[blues[i]]++;
					}
				}
			}
			int maxBRed = 0, maxWRed = 0, maxBGreen = 0, maxWGreen = 0, maxBBlue = 0, maxWBlue = 0; //�������������� ���� � ��� �� ������� ����� (����� ������ �����)

			int minPix = 0;
			int maxPix = 255;
			int sum = 0;
	#pragma omp parallel sections default(none) private(sum, minPix, maxPix) shared(maxWRed, maxWGreen, maxWBlue, maxBRed, maxBGreen, maxBBlue, sortRed, sortGreen, sortBlue)
			{// ����� k-�� ���������� ���������� �� ������� ����� ������ � ����� (W(white) ������, B(black) ����� 
				#pragma omp section
				{
					minPix = 0;
					sum = 0;
					while (sum < partMaxBW && minPix < 256) {
						sum += sortRed[minPix];
						minPix++;
					}
					maxBRed = max(0, minPix - 1);
				}
				#pragma omp section
				{
					minPix = 0;
					sum = 0;
					while (sum < partMaxBW && minPix < 256) {
						sum += sortGreen[minPix];
						minPix++;
					}
					maxBGreen = max(0, minPix - 1);
				}
				#pragma omp section
				{
					minPix = 0;
					sum = 0;
					while (sum < partMaxBW && minPix < 256) {
						sum += sortBlue[minPix];
						minPix++;
					}
					maxBBlue = max(0, minPix - 1);
				}
				#pragma omp section
				{
					maxPix = 255;
					sum = 0;
					while (sum < partMaxBW && maxPix >= 0) {
						sum += sortRed[maxPix];
						maxPix--;
					}
					maxWRed = min(255, maxPix + 1);
				}
				#pragma omp section
				{
					maxPix = 255;
					sum = 0;
					while (sum < partMaxBW && maxPix >= 0) {
						sum += sortGreen[maxPix];
						maxPix--;
					}
					maxWGreen = min(255, maxPix + 1);
				}
				#pragma omp section
				{
					maxPix = 255;
					sum = 0;
					while (sum < partMaxBW && maxPix >= 0) {
						sum += sortBlue[maxPix];
						maxPix--;
					}
					maxWBlue = min(255, maxPix + 1);
				}
			}
			outReds = new unsigned char[countPixels];
			outGreens = new unsigned char[countPixels];
			outBlues = new unsigned char[countPixels];
			int MAX = max(max(maxWRed, maxWGreen), maxWBlue); //������� ����� �������� � �������
			int MIN = min(min(maxBRed, maxBGreen), maxBBlue);
			if (MAX == MIN) {
#pragma omp parallel for schedule(static) default(none) shared(countPixels, outReds, outGreens, outBlues, reds, greens, blues, MIN)
				for (int i = 0; i < countPixels; i++) {
					if (reds[i] <= MIN) {
						outReds[i] = 0;
					}
					else {
						outReds[i] = 255;
					}
					if (greens[i] <= MIN) {
						outGreens[i] = 0;
					}
					else {
						outGreens[i] = 255;
					}
					if (blues[i] <= MIN) {
						outBlues[i] = 0;
					}
					else {
						outBlues[i] = 255;
					}
				}
			}
			else {
				float m = 255.0 / (MAX - MIN); // ��� �������� �������� ��������� ����� ������� ��������� 
				float d = m * MIN;
#pragma omp parallel for schedule(static) default(none) shared(m, d, sortRed, sortGreen, sortBlue)
				for (int i = 0; i < 256; i++) { //��������� ������� ���� ������� (�� ��� ��������� ��������)
					sortRed[i] = min(255, max(0, int(round(i * m - d)))); //��� �������� ���������� � �� �� �������
					sortGreen[i] = min(255, max(0, int(round(i * m - d))));
					sortBlue[i] = min(255, max(0, int(round(i * m - d))));
				}
#pragma omp parallel for schedule(static) default(none) shared(countPixels, outReds, outGreens, outBlues, sortRed, sortGreen, sortBlue, reds, greens, blues)
				for (int i = 0; i < countPixels; i++) { //��������� ������ �������
					outReds[i] = sortRed[reds[i]];
					outGreens[i] = sortGreen[greens[i]];
					outBlues[i] = sortBlue[blues[i]];
				}
			}
			TimeFinish = clock();
			delete[] reds;
			delete[] greens;
			delete[] blues;
			delete[] sortRed;
			delete[] sortGreen;
			delete[] sortBlue;
		}
	}
	catch (exception e) { //�������� �� ������������ ������ � ���� ������ ��-�� ������������� �����
		cout << "FileInput uncorrectly" << endl;
		cout << "FileInput must be coorectly P6(PPM) or P5(PGM) without comments";
		return -2;
	}
//�����
	ofstream outImage;
	outImage.open(fileOutputName, ios::binary);
	if (!outImage.is_open()) {
		cout << "This output file " << fileOutputName << "is incorrect or not found";
		return 1;
	}
	outImage << typeFile << endl;
	outImage << width << " " << height << endl;
	outImage << 255 << endl;
	if (typeFile == "P5") {
		for (int i = 0; i < countPixels; i++) {
			outImage << outGreys[i];
		}
	}
	else {
		for (int i = 0; i < countPixels; i++) {
			outImage << outReds[i] << outGreens[i] << outBlues[i];
		}
	}
	outImage.close();
	printf("Time (%i thread(s)) : %g ms\n", countThreads, TimeFinish - TimeStart); //����� (�� ctime) � ��
	delete[] outReds;
	delete[] outBlues;
	delete[] outGreens;
	delete[] outGreys;
	return 0;
}