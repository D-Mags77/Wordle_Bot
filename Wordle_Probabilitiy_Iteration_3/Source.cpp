#include <iostream>
#include <stdio.h>
#include <string>
#include <vector>
#include <fstream>
#include <random>
#include <map>
#include <numeric> 
#include <algorithm>
#include <chrono>
#include <unordered_map> 
#include <cmath>
#include <iomanip>
#include <omp.h>

using namespace std;
using namespace std::chrono;


vector<string> WordListVector;
vector<string> WordListVectorCOPY;
vector<int> finalResult;
vector<double> ProbabilityOfWord;
vector<double> TempProbability;
vector<vector<int>> combinations;


double log2(double x) {
	return log(x) / log(2);
}

vector<vector<int>> generateCombinations() {
	for (int i = 0; i < 3; ++i) {
		for (int j = 0; j < 3; ++j) {
			for (int k = 0; k < 3; ++k) {
				for (int l = 0; l < 3; ++l) {
					for (int m = 0; m < 3; ++m) {
						vector<int> currentCombination = { i, j, k, l, m };						
						combinations.push_back(currentCombination);
					}
				}
			}
		}
	}
	return combinations;
}

void ReadInData() {
	ifstream inputFile("12,953_Words.txt");

	string Word;
	int count = 0;

	if (inputFile.is_open()) {

		do {

			inputFile >> Word;
			WordListVector.push_back(Word);
			//cout << Word << endl;
			if (!inputFile.fail()) {
				count++;
			}


		} while (!inputFile.fail());
		inputFile.close();


	}
	else {
		cout << "Could not open file" << endl;
	}

	
}

void LetterRemoval(string NextGuess) {
    double initialWordCount = WordListVectorCOPY.size();
    vector<string> newWordList;

    for (const auto& word : WordListVectorCOPY) {
        bool valid = true;

        for (int i = 0; i < finalResult.size(); i++) {
            if ((finalResult[i] == 0 && word.find(NextGuess[i]) != string::npos) || 
                (finalResult[i] == 1 && word[i] != NextGuess[i]) || 
                (finalResult[i] == 2 && word[i] == NextGuess[i])) {
                valid = false;
                break;
            }
        }

        if (valid) {
            newWordList.push_back(word);
        }
    }

    WordListVectorCOPY = move(newWordList); // Update the original list
    double remainingWordCount = WordListVectorCOPY.size();
    double Probability = remainingWordCount / initialWordCount;
    double LogFactor = log2(1 / Probability);
    double FinalResult = Probability * LogFactor;
    TempProbability.push_back(FinalResult);
}

void ProbabilityCalculationsFinal() {
	double sum = 0.0;
	for (double prob : TempProbability) {
		if (!isnan(prob)) {
			sum += prob;
		}
	}
	// cout << "Sum of TempProbability: " << sum << endl;
	ProbabilityOfWord.push_back(sum);
	TempProbability.clear();
}

int main() {
	
	ReadInData();
	auto start = high_resolution_clock::now();
	
	vector<vector<int>> combinations = generateCombinations(); // all of this is working you just need to make the probability happen

	for (int i = 0; i < WordListVector.size(); i++) { //goes through every word to calculate
		//WordListVectorCOPY = WordListVector; //this is choosing the first word that the probability will be calculated
		//string WordToCalculate = WordListVectorCOPY[i];
		vector<string> localWordListCOPY = WordListVector; // Local copy for each thread
		string WordToCalculate = localWordListCOPY[i];
		cout << WordToCalculate << " " << i << endl;


		for (size_t k = 0; k < combinations.size(); k++) { //goes through each instance  of combinations

			finalResult = combinations[k];
			
			WordListVectorCOPY = WordListVector;
			

			LetterRemoval(WordToCalculate);




		}
		#pragma omp critical 
		ProbabilityCalculationsFinal();

		
	}

	ofstream outFile("12,953_Probabilities.txt");

	if (outFile.is_open()) {
		for (size_t i = 0; i < ProbabilityOfWord.size(); ++i) {
			outFile << fixed << setprecision(8) << ProbabilityOfWord[i] << "\n";
		}


		

		outFile.close();
		cout << "Data written to output.txt" << endl;
	}
	else {
		cerr << "Unable to open file!" << endl;
	}

	return 0;

}