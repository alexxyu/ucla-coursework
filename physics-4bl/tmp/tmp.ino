#include <math.h>
// enumerating 3 major temperature scales
enum {
  T_KELVIN=0,
  T_CELSIUS,
  T_FAHRENHEIT
};

#define EPISCO_K164_10k 4300.0f,298.15f,50000.0f  // B,T0,R0  

float Temperature(int AnalogInputNumber,int OutputUnit,float B,float T0,float R0,float R_Balance)
{
  float R,T;

//  R=1024.0f*R_Balance/float(analogRead(AnalogInputNumber)))-R_Balance;
  R=R_Balance*(1024.0f/float(analogRead(AnalogInputNumber))-1);

  T=1.0f/(1.0f/T0+(1.0f/B)*log(R/R0));

  switch(OutputUnit) {
    case T_CELSIUS :
      T-=273.15f;
    break;
    case T_FAHRENHEIT :
      T=9.0f*(T-273.15f)/5.0f+32.0f;
    break;
    default:
    break;
  };

  return T;
}

//example of use #2
// using numbers instead of episco k164 definition
// this time reading from analog input 2
// getting result in fahrenheit

void setup() {
 Serial.begin(9600);
}

void loop() {

 Serial.println("********");
 Serial.println("Temp");
 Serial.println(Temperature(2,T_FAHRENHEIT,4300.0f,298.15f,50000.0f,10000.0f));
 Serial.println("********");
 Serial.println(" ");

 delay(500);
}
