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

#define LATCH 2 //  aka OUT
#define CLOCK 3

#define IN_BITS 40
#define OUT_BYTES 4

volatile byte bitBuffer[IN_BITS];
volatile byte bitPtr;

volatile byte outBuffer[OUT_BYTES];
volatile byte outPtr;

const byte BUTTONS[8] = {
    RIGHT, LEFT, DOWN, UP, START, SELECT, B, A,

};

void reset() {
  bitPtr = 0;
  outPtr = 0;
  for (int i = 0; i < IN_BITS; i++) {
    bitBuffer[i] = 0;
  }
  for (int i = 0; i < OUT_BYTES; i++) {
    outBuffer[i] = 0;
  }
}

void setControllerOutput(byte output) {
  byte out = output ^ 0xFF;
  for (int i = 0; i < 8; i++) {
    digitalWrite(BUTTONS[i], out & 1);
    out = out >> 1;
  }
}

void setup() {
  reset();
  setControllerOutput(0);

  Serial.begin(9600);

  pinMode(RIGHT, OUTPUT);
  pinMode(LEFT, OUTPUT);
  pinMode(UP, OUTPUT);
  pinMode(DOWN, OUTPUT);
  pinMode(SELECT, OUTPUT);
  pinMode(START, OUTPUT);
  pinMode(A, OUTPUT);
  pinMode(B, OUTPUT);

  pinMode(LATCH, INPUT);
  pinMode(CLOCK, INPUT);
  while (!Serial)
    ;
}

void advanceStep() {
  setControllerOutput(outBuffer[outPtr]);
  outPtr++;
}

void readSingleBit() {
  bitBuffer[bitPtr] = digitalRead(LATCH);
  bitPtr++;
}

void loop() {
  reset();
  attachInterrupt(digitalPinToInterrupt(LATCH), advanceStep, RISING);
  while (outPtr < OUT_BYTES)
    ;
  detachInterrupt(digitalPinToInterrupt(LATCH));
  attachInterrupt(digitalPinToInterrupt(CLOCK), readSingleBit, FALLING);
  while (bitPtr < IN_BITS)
    ;
  detachInterrupt(digitalPinToInterrupt(CLOCK));
  for (int i = 0; i < (IN_BITS / 8); i++) {
    byte value = 0;
    for (int j = 0; j < 8; j++) {
      value |= bitBuffer[i * 8 + j] << j;
    }
    Serial.print(value >> 4, HEX);
    Serial.print(value & 0xF, HEX);
    if (i < (IN_BITS / 8))
      Serial.print(" ");
  }
  Serial.println(String(outPtr));
  Serial.println(String(bitPtr));

  // read bytes into the bit buffer here
}
