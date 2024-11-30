#include <iostream>
#include <stdio.h>
#include <string>
#include <vector>
#include <random>
#include <map>
#include <numeric> 
#include <algorithm>
#include <chrono>
#include <unordered_map> 
#include <iomanip>  
#include <utility>
#include <iterator>
#include <fstream>
#include <cmath>


using namespace std;

vector<int> finalResult;
vector<string>WordListVectorFINAL;
vector<string> WordListVectorCOPY;

void ReadInData() {
	ifstream inputFile("12,953_Words.txt");
	string Word;
	int count = 0;

	if (inputFile.is_open()) {

		do {
			inputFile >> Word;
			WordListVectorFINAL.push_back(Word);

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

	ifstream inputFile2("12,953_Probabilities.txt");
}

void LetterRemoval(string NextGuess) {


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
	cout << "Total Words Left: " << WordListVectorCOPY.size() << endl;
}

int main() {

	
	ReadInData();
	
	bool Restart = false;
	
	while (Restart == false) {



		bool EndGame = false;


		WordListVectorCOPY = WordListVectorFINAL;

		while (EndGame == false) {


			int input;
			string tempword;
			cout << "Enter the word you played into Worlde then enter the string of numbers associated with Wordle: 1 = Green, 2 = Yellow, 0 = Gray. Gray Gray Gray Green Yellow would be 00012: " << endl;
			cout << "Word : ";
			cin >> tempword;
			cout << endl << "Sequence of numbers: ";
			cin >> input;


			for (int i = 0; i < 5; ++i) {
				finalResult.insert(finalResult.begin(), input % 10);  // Get the last digit and insert at the beginning
				input /= 10;  // Remove the last digit
			}

			LetterRemoval(tempword);

			if (WordListVectorCOPY.size() == 0) {
				cout << "There are no words which satisfy what you have inputted " << endl;
				return 0;
			}

			cout << "The next guess you should play is " << WordListVectorCOPY[0] << endl;


			if (all_of(finalResult.begin(), finalResult.end(), [](int value) { return value == 1; })) {
				EndGame == true;
				WordListVectorCOPY = WordListVectorFINAL;

			}

			finalResult.clear();


		}
		
	}
		





	
	return 0;
}