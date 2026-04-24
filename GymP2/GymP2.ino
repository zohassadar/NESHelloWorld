#include <SPI.h>
#include <Wire.h>

// pins
#define A 4
#define B 5
#define SELECT 6
#define START 7
#define UP 8
#define DOWN 9
#define LEFT 10
#define RIGHT 11

#define LATCH 2   //  aka OUT
#define CLOCK 3

volatile byte data[256];
volatile byte ptr;


void setup() {
    // for (int i = 0; i < 256; i++) {
    //   data[i] = 0;
    //   }
    Serial.begin(9600);

    // pinMode(RIGHT, OUTPUT);
    // pinMode(LEFT, OUTPUT);
    // pinMode(UP, OUTPUT);
    // pinMode(DOWN, OUTPUT);
    // pinMode(SELECT, OUTPUT);
    // pinMode(START, OUTPUT);
    // pinMode(A, OUTPUT);
    // pinMode(B, OUTPUT);
    pinMode(LATCH, INPUT);
    pinMode(CLOCK, INPUT);

    attachInterrupt(digitalPinToInterrupt(CLOCK),read_latch, FALLING);
    //
    // digitalWrite(LEFT, 1);
    // digitalWrite(RIGHT, 1);
    // digitalWrite(UP, 1);
    // digitalWrite(DOWN, 1);
    // digitalWrite(START, 1);
    // digitalWrite(SELECT, 1);
    // digitalWrite(A, 1);
    // digitalWrite(B, 1);
    //
    while (!Serial); // wait for Serial connection (arduino leonardo)
}

void read_latch() {
    data[ptr] = digitalRead(LATCH);
      ptr++;
}


void loop() {
    // long sum = 0;
    // for (int i = 1; i < 256; i++){
    //   sum = sum + (data[(ptr+i)%256] - data[(ptr+(i-1))%256]);
    // }
    for (int i = 0; i < 32; i++){
      byte value = 0;
      for (int j = 0; j < 8; j++){
        value |= data[i * 8 + j] << j;
      }
      if (value < 0x10) Serial.print("0");
      Serial.print(value, HEX);
      if (i < 31) Serial.print(" ");

    }
    Serial.println();
    delay(1000);
    // Serial.println(String(clocks));
}
