int ThermistorPin = 0;
int Vo;
float R1 = 10000;
float logR2, R2, T;
float c1 = 2.055804316e-03, c2 = 0.5779987905e-04, c3 = 5.356844641e-07;

void setup() {
  Serial.begin(115200);
}

void loop() {

  Vo = analogRead(ThermistorPin);
  R2 = R1 * (1023.0 / (float)Vo - 1.0);
  Serial.println(R2);
  
  logR2 = log(R2);
  T = (1.0 / (c1 + c2*logR2 + c3*logR2*logR2*logR2));
  T = T - 273.15;
  // T = (T * 9.0)/ 5.0 + 32.0; 

  // Serial.print("Temperature: "); 
  // Serial.println(T);
  // Serial.println(" F"); 

  delay(50);
}
