#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <iostream>
#include "palasm.h"

using namespace ::testing;
using std::iostream;
using std::cout;
using std::endl;

class TestPalasm : public Palasm, public Test {
public:
    TestPalasm() {

    }
   
};

// Demonstrate some basic assertions.
TEST_F(TestPalasm, ChkSum) {
  // Expect two strings not to be equal.
  //EXPECT_STRNE("hello", "world");
  // Expect equality.
  //EXPECT_EQ(7 * 6, 42);
  
  sumchk();
  int chk0_ok[] = {0, 77, 0, 77};
  for (size_t tt = 0; tt < 4; tt++) {
  
    ASSERT_TRUE(chk0_ok[tt] == _isum[tt]);
  }

}