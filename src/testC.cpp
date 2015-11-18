#include <iostream>

using namespace std;

extern"C" {
void fortfunc1_(int *ii, float *ff);
void fortfunc2_();
void paranmrd_();
}

main()
{

   int ii=5;
   float ff=5.5;

   fortfunc1_(&ii, &ff);
   fortfunc2_();
   paranmrd_();

   return 0;
}