#include <iostream>
#include <fstream>
#include <string>

using namespace std;

int main (int argc, char **argv) {

  ifstream infile (argv[1]);
  string s;

  if (! infile.is_open ())
    cerr << "error opening " << argv[1] << "\n";
  else
    {
      while (infile.good())
	{
	  getline (infile, s);
	  cout << "here: " << s << "\n";
	}
    
      infile.close();
    }

  return 0;
}
