#include <Wire.h>
#include <inttypes.h>
#include "DmxSimple.h"

/*****************************************************************************
 * constants
 *****************************************************************************/

// pins
#define LED_PIN        13
#define DMX_TX_PIN      1
#define DMX_RX_PIN      0
#define DMX_TX_EN_PIN   3

// COBS 
#define OVERHEAD 2 // from strings up to 254 bytes in length

// current hardcoded to the highest DMX channel we use
#define MAX_CHANNEL 64

/*****************************************************************************
 * allLightsOff
 *****************************************************************************/

void allLightsOff ()
 {
  for (int i = 0; i < MAX_CHANNEL; i++) {
    DmxSimple.write(i, 0);
  }
 }

/*****************************************************************************
 * initDMX
 *****************************************************************************/

void initDMX () {
  DmxSimple.usePin(DMX_TX_PIN);
  DmxSimple.maxChannel(MAX_CHANNEL);

  pinMode(DMX_TX_EN_PIN, OUTPUT);
  digitalWrite (DMX_TX_EN_PIN, HIGH);

  allLightsOff();

  return;
}

/*****************************************************************************
 * unstuff
 *****************************************************************************/

// unstuff data (Consistent Overhead Byte Stuffing,
// http://en.wikipedia.org/wiki/Consistent_Overhead_Byte_Stuffing)

void unstuff(const unsigned char *src, unsigned char *dst) {
    const unsigned char *end = src + MAX_CHANNEL + OVERHEAD;
    while (src < end) {
        int i, code = *src++;
        for (i=1; i < code; i++)
            *dst++ = *src++;
        if (code < 0xFF)
            *dst++ = 0;
    }
}

/*****************************************************************************
 * setup
 ****************************************************************************/

void setup() {
  pinMode(LED_PIN, OUTPUT);
   // flash once to show we turned on
    digitalWrite(LED_PIN, HIGH);
    delay(200);
    digitalWrite(LED_PIN, LOW);

  Serial.begin(115200); // FIXME: baud rate seems a bit low ?
  initDMX();
  DmxSimple.write(1, 255);
  delay(100);
  DmxSimple.write(1, 0);

}

/*****************************************************************************
 * loop
 ****************************************************************************/

unsigned char buf[MAX_CHANNEL];
unsigned char unstuffed[MAX_CHANNEL];
unsigned char *ptr=buf;
unsigned char *end = buf + MAX_CHANNEL + 1;
int state = 0;
int toggle = 255;

void loop () {
  int val;
  while (Serial.available()) {
    val = Serial.read();
    //    Serial.print(val);
    //     if (val == -1) break;


    switch (state) {
    case 0:  // normal state

      if (ptr > end) { // frame was too long
        state = 1;
        DmxSimple.write(2, 255);
        break;
      }

      *ptr++ = val;
      if (!val) { // got end of frame
        //        DmxSimple.write(1, toggle);
        //        if (toggle) toggle = 0; else toggle = 255;

        ptr = buf;
        unstuff(buf, unstuffed);
        for (int c = 1; c <= MAX_CHANNEL; c++) {
          DmxSimple.write(c, unstuffed[c-1]);
        }

      }
      break;
    case 1:
      if (val == 0) {
        ptr = buf;
        state = 0;
        break;
      }
    }
  }
}

#if 0
void loop () {
    byte val;
#if 0
        DmxSimple.write(1, 255);
        DmxSimple.write(3, 0);
        delay (500);
        DmxSimple.write(3, 255);
        DmxSimple.write(1, 0);
        delay (500);

#else
    switch(state) {
#if 0
    case 0: // waiting for start of frame
#if 1
      DmxSimple.write(1, 255);
      DmxSimple.write(2, 0);
      DmxSimple.write(3, 0);
#endif
            while (Serial.available()) {
                val = Serial.read();
                if (val == 0)
                {
                    ptr = buf;
                    state = 1;
                }
            };
            break;
#endif
    case 1: // filling buffer
#if 1
      DmxSimple.write(1, 0);
      DmxSimple.write(2, 255);
      DmxSimple.write(3, 0);
#endif
        while (Serial.available()) {
          val = Serial.read();
          *ptr++ = val;
          if (!val) {
            state = 2;
            ptr = buf;
            
            break;
          } else {
            if (ptr >= end) { // frame was too long
              ptr = buf;
            }
          }
        }
        break;

    case 2:
#if 1
      DmxSimple.write(1, 0);
      DmxSimple.write(2, 0);
      DmxSimple.write(3, 255);
      delay(100);
#endif
        state = 1;
        unstuff(buf, unstuffed);
        for (int c = 1; c <= MAX_CHANNEL; c++) {
          //          DmxSimple.write(c, toggle);

          //           DmxSimple.write(c, unstuffed[c-1]);
        }
        if (toggle) toggle = 0; else toggle = 255;
        break;
    }
#endif
}
#endif
