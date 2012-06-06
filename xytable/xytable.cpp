// xytable.cpp
// generates a lookup table for all the cells of the connect-four grid

#include <stdio.h>
#include <iostream>
using namespace std;

int main()
{
  int width = 7;
  int height = 6;

  cerr << "Generating lookup table for " << width << " by " << height << " grid." << endl;
  
  int cellNum = 0;
  for(int x = 0; x < width; x++)
    {
      for(int y = 0; y < height; y++)
	{
	  printf("getCellXY(%2d,%2d,%2d). ",cellNum,x,y);
	  //	  cout << "getCellXY(" << cellNum << "," << x << "," << y << "). ";
	  cellNum++;
	}
      cout << endl;
    }
  
}
