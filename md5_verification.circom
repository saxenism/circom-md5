pragma circom 2.1.6;

include "node_modules/circomlib/circuits/gates.circom";
include "node_modules/circomlib/circuits/comparators.circom";

template Combine() {}

template Md5() {
    // since the word size in the MD5 algo is 32 bits and the input size is 512, let's create an input signal of 16 elements.
    signal input in[16];
    signal input out[4]; // The hash result

    ////////////////////////////////
    /// Range Check on the input in
    ////////////////////////////////
    // We need to constrain the input signals to be less than 2**32.
    component isLessThan[16];
    signal rangeCheckArray[16];

    for(var i = 0; i < 16; i++) {
        isLessThan[i] = LessThan(252);

        isLessThan[i].in[0] <== in[i];
        isLessThan[i].in[1] <== 2**32;

        rangeCheckArray[i] <== isLessThan[i].out;
    }

    // Now we need to make sure that all the entries in the rangeCheckArray are 1.
    // So, we will do an AND of all the value in rangeCheckArray and constraint that output to be equal to 1.
    component multipleANDs = MultiAND(16);
    multipleANDs.in <== rangeCheckArray;
    multipleANDs.out === 1;

    /////////////////////////////////////////////
    /// Loop iterations for state change starts
    /////////////////////////////////////////////

    signal states[64][4]; // since there are going to be 64 states and each state plays with 4 values.

    // Initial state value is same for all MD5 hashes
    states[0][0] <== 0x67452301;
    states[0][1] <== 0xefcdab89;
    states[0][2] <== 0x98badcfe;
    states[0][3] <== 0x10325476;

    // First loop of state changes
    for(var i = 1; i < 16; i++) {
        states[i][0] <== states[i-1][3];
        states[i][1] <== Combine();
        states[i][2] <== states[i-1][1];
        states[i][3] <== states[i-1][2];
    }

    // Second loop of state changes
    for(var j = 16; j < 32; j++) {
        var i = (5 * i + 1) % 16;

        states[i][0] <== states[i-1][3];
        states[i][1] <== Combine();
        states[i][2] <== states[i-1][1];
        states[i][3] <== states[i-1][2];
    }

    // Third loop of state changes
    for(var l = 32; l < 48; l++) {
        var i = (3 * i + 5) % 16;

        states[i][0] <== states[i-1][3];
        states[i][1] <== Combine();
        states[i][2] <== states[i-1][1];
        states[i][3] <== states[i-1][2];
    }

    // Fourth loop of state changes
    for(var k = 48; k < 64; k++) {
        var i = (7 * i) % 16;

        states[i][0] <== states[i-1][3];
        states[i][1] <== Combine();
        states[i][2] <== states[i-1][1];
        states[i][3] <== states[i-1][2];
    }

    out[0] <== states[63][0];
    out[1] <== states[63][1];
    out[2] <== states[63][2];
    out[3] <== states[63][3];

}

component main{ public [ out ] } = Md5();