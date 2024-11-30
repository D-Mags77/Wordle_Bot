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
	ifstream inputFile("5757_Words.txt");

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

	/*for (int i = 0; i < WordListVector.size(); i++) {
		cout << WordListVector[i] << endl;
	}
	cout << count << endl;

	system("pause");
	*/
}

void LetterRemoval(string NextGuess) {
	// cout << "Words before letter removals: " << WordListVectorCOPY.size() << endl;
	double initialWordCount = WordListVectorCOPY.size();


	for (int i = 0; i < finalResult.size(); i++) {

		if (finalResult[i] == 0) {
			char letterToRemove = NextGuess[i];
			WordListVectorCOPY.erase(
				remove_if(WordListVectorCOPY.begin(), WordListVectorCOPY.end(),
					[letterToRemove](const string& word) {
						return word.find(letterToRemove) != string::npos;
					}),
				WordListVectorCOPY.end());
		}

		if (finalResult[i] == 2) {
			char temporaryLetter = NextGuess[i];
			WordListVectorCOPY.erase(
				remove_if(WordListVectorCOPY.begin(), WordListVectorCOPY.end(),
					[i, temporaryLetter](const string& word) {
						return word[i] == temporaryLetter;
					}),
				WordListVectorCOPY.end());
		}

		if (finalResult[i] == 1) {
			char correctLetter = NextGuess[i];
			WordListVectorCOPY.erase(
				remove_if(WordListVectorCOPY.begin(), WordListVectorCOPY.end(),
					[i, correctLetter](const string& word) {
						return word[i] != correctLetter;
					}),
				WordListVectorCOPY.end());
		}

		if (finalResult[i] == 2) {
			char correctLetter = NextGuess[i];
			WordListVectorCOPY.erase(
				remove_if(WordListVectorCOPY.begin(), WordListVectorCOPY.end(),
					[correctLetter](const string& word) {
						return word.find(correctLetter) == string::npos;
					}),
				WordListVectorCOPY.end());
		}
	}

	// cout << "Words after letter removals: " << WordListVectorCOPY.size() << endl;
	double remainingWordCount = WordListVectorCOPY.size();

	double Probability = remainingWordCount / initialWordCount;
	// cout << Probability << endl;
	double LogFactor = log2(1 / Probability);
	double FinalResult = Probability * LogFactor;
	TempProbability.push_back(FinalResult);
	// cout << "This is the probability " << FinalResult << endl;

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
	//cout << FINALPROBABILITY.size();
	//system("pause");
	ReadInData();
	auto start = high_resolution_clock::now();
	vector<vector<int>> combinations = generateCombinations(); // all of this is working you just need to make the probability happen

	for (int i = 0; i < WordListVector.size(); i++) { //goes through every word to calculate
		WordListVectorCOPY = WordListVector; //this is choosing the first word that the probability will be calculated
		string WordToCalculate = WordListVectorCOPY[i];
		cout << WordToCalculate << " " << i << endl;


		for (size_t k = 0; k < combinations.size(); k++) { //goes through each instance  of combinations

			finalResult = combinations[k];
			WordListVectorCOPY = WordListVector;

			/*	for (int l = 0; l < finalResult.size(); l++) {
					cout << finalResult[l];
				}
				cout << endl;
				*/

			LetterRemoval(WordToCalculate);




		}
		ProbabilityCalculationsFinal();

		/*   for (int m = 0; m < TempProbability.size(); m++) {
			   cout << TempProbability[m] << endl;
		   }


		   cout << "This is the final probability List" << endl;
		   for (int n = 0; n < ProbabilityOfWord.size(); n++) {
			   cout << ProbabilityOfWord[n] << endl;
		   }
		 */
	}

	ofstream outFile("5757_Probabilities.txt");

	if (outFile.is_open()) {
		for (size_t i = 0; i < ProbabilityOfWord.size(); ++i) {
			outFile << fixed << setprecision(8) << ProbabilityOfWord[i];
			if (i != ProbabilityOfWord.size() - 1) {
				outFile << ", ";
			}
		}


		auto end = chrono::high_resolution_clock::now();
		chrono::duration<double> elapsed = end - start;
		outFile << "\nProgram runtime: " << elapsed.count() << " seconds";

		outFile.close();
		cout << "Data written to output.txt" << endl;
	}
	else {
		cerr << "Unable to open file!" << endl;
	}

	return 0;

}