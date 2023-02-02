#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <cmath>
#include <omp.h>
#include <ctime>
using namespace std;

int main(int argc, char const *argv[])
{
	int countThreads = 0;
	float ratio = 0.0;
	int height = 0;
	int width = 0;
	int countPixels = 0;
	int partMaxBW = 0;
	unsigned char *outReds = new unsigned char[1];
	unsigned char *outGreens = new unsigned char[1];
	unsigned char *outBlues = new unsigned char[1];

	if (argc != 5) {
		cout << " You must enter your input in format: <countThreads> <fileInputName> <fileOutputName> <ratio>" << endl;
		cout << "fileInput must be P6(PPM)" <<endl;
		return 1;
	}

	string str1 = argv[1];
	string str2 = argv[4];
	string fileInputName = argv[2];
	string fileOutputName = argv[3];
	try {
		countThreads = stoi(str1);
		if (countThreads < 0) {
			cout << "countThreads must be >= 0";
			return 1;
		} else if (countThreads != 0) {
			omp_set_num_threads(countThreads);
		}
	} catch(invalid_argument e) {
		cout << "countThreads isn't integer" << endl;
		return 1;
	}

	try {
		ratio = stof(str2);
		if (ratio >= 0.5 || ratio < 0) {
			cout << "Ratio must be in range [0; 0.5)"<< endl;
			return 1;
		}
	} catch(invalid_argument e) {
		cout << "Ratio isnt float" << endl;
		return 1;
	}
	ifstream inImage;
	inImage.open(fileInputName, ios::binary);
	if (!inImage.is_open()) {
		cout << "File: " + string(fileInputName) + "cannot read";
		return 1;
	}


	string typeFile = "";
	getline(inImage, typeFile);
	if (typeFile != "P6") {
		cout << "fileInput isnt P6(PPM)";
		inImage.close();
		return 1;
	}

	string strBuf = "";
	char ch;
	while(!inImage.eof()) {
		inImage.get(ch);
		if (!isdigit(ch)) {
			break;
		} else {
			strBuf += ch;
		}
	}
	width = stoi(strBuf);
	strBuf = "";

	while(!inImage.eof()) {
		inImage.get(ch);
		if (!isdigit(ch)) {
			break;
		} else {
			strBuf += ch;
		}
	}
	height = stoi(strBuf);
	if (height <= 0 || width <= 0) {
		cout << "fileInput uncorrectly, width and height must be > 0" << endl;
		return 1;
	}
	countPixels = width * height; // общее кол-во пикселей
	partMaxBW = int(ratio * countPixels);


	strBuf = "";

	while(!inImage.eof()) {
		inImage.get(ch);
		if (!isdigit(ch)) {
			break;
		} else {
			strBuf += ch;
		}
	}
	int maxBright = stoi(strBuf);
	if (maxBright != 255) {
		cout << "fileInput uncorrectly, maximum bright must be = 255";
		return 1;
	}
	// считывание хедера закончилось


	// считывание пикселей по цветам ргб
	unsigned char *reds = new unsigned char[countPixels];
	unsigned char *greens = new unsigned char[countPixels];
	unsigned char *blues = new unsigned char[countPixels];
//считывание
	for (int i = 0; i < 3 * countPixels; i++) {
		inImage.get(ch);
		if (i % 3 == 0) {
			reds[i / 3] = ch;
		}

		if (i % 3 == 1) {
			greens[i / 3] = ch;
		}

		if (i % 3 == 2) {
			blues[i / 3] = ch;
		}
	}

//сортировка
	int start = clock();
	int *sortRed = new int[256];
	int *sortGreen = new int[256];
	int *sortBlue = new int[256];
	for (int i = 0; i < 256; i++) {
		sortRed[i] = 0;
		sortGreen[i] = 0;
		sortBlue[i] = 0;
	}

	for (int i = 0; i < countPixels; i++) {
		sortRed[reds[i]]++;
		sortGreen[greens[i]]++;
		sortBlue[blues[i]]++;
	}

	int maxBRed = 0, maxWRed = 0, maxBGreen = 0, maxWGreen = 0, maxBBlue = 0, maxWBlue = 0;
	int minPix = 0;
	int maxPix = 255;
	int sum = 0;
	#pragma omp parallel sections private(sum, minPix, maxPix) shared(maxWRed, maxWGreen, maxWBlue, maxBRed, maxBGreen, maxBBlue)
	{
		#pragma omp section
		{
			sum = 0;
			minPix = 0;
			while (sum < partMaxBW && minPix < 256) {
				sum += sortRed[minPix];
				minPix++;
			}
			maxBRed = max(0, minPix - 1);
		}
		#pragma omp section
		{
			sum = 0;
			minPix = 0;
			while (sum < partMaxBW && minPix < 256) {
				sum += sortGreen[minPix];
				minPix++;
			}
			maxBGreen = max(0, minPix - 1);
		}
		#pragma omp section
		{
			sum = 0;
			minPix = 0;
			while (sum < partMaxBW && minPix < 256) {
				sum += sortBlue[minPix];
				minPix++;
			}
			maxBBlue = max(0, minPix - 1);
		}
		#pragma omp section
		{	
			sum = 0;
			maxPix = 255;
			while (sum < partMaxBW && maxPix >= 0) {
				sum += sortRed[maxPix];
				maxPix--;
			}
			maxWRed = min(255, maxPix + 1);
		}
		#pragma omp section
		{
			sum = 0;
			maxPix = 255;
			while (sum < partMaxBW && maxPix >= 0) {
				sum += sortGreen[maxPix];
				maxPix--;
			}
			maxWGreen = min(255, maxPix + 1);
		}
		#pragma omp section
		{
			sum = 0;
			maxPix = 255;
			while (sum < partMaxBW && maxPix >= 0) {
				sum += sortBlue[minPix];
				maxPix--;
			}
			maxWBlue = min(255, maxPix + 1);
		}
	}
	int MAX = max(max(maxWRed, maxWGreen), maxWBlue);
	int MIN = min(min(maxBRed, maxBGreen), maxBBlue);
	outReds = new unsigned char[countPixels];	
	outGreens = new unsigned char[countPixels];
	outBlues = new unsigned char[countPixels];
	float m = 255/(MAX - MIN);
	float d = m * MIN;
	#pragma omp parallel for 
	for (int i = 0; i < 256; i++) {
		sortRed[i] = min(255, max(0, int(round(i * m - d))));
		sortGreen[i] = min(255, max(0, int(round(i * m - d))));
		sortBlue[i] = min(255, max(0, int(round(i * m - d))));
	}
	#pragma omp parallel for
	for (int i = 0; i < countPixels; i++) {
		outReds[i] = sortRed[reds[i]];
		outGreens[i] = sortGreen[greens[i]];
		outBlues[i] = sortBlue[blues[i]];
	}
	int finish = clock();
//вывод
	ofstream outImage;
	outImage.open(fileOutputName, ios::binary);

	if (!outImage.is_open()) {
		cout << "File: " + string(fileOutputName) + "cannot read";
	}
	outImage << "P6" << endl;
	outImage << width << " " << height << endl;
	outImage << maxBright << endl;
	for (int i = 0; i < countPixels; i++) {
		outImage << outReds[i] << outGreens[i] << outBlues[i];
	}
	cout << (finish - start) << endl;
	delete[] reds;
	delete[] greens;
	delete[] blues;
	delete[] sortRed;
	delete[] sortGreen;
	delete[] sortBlue;
	delete[] outReds;
	delete[] outGreens;
	delete[] outBlues;
	return 0;
}