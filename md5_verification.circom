pragma circom 2.1.6;

include "node_modules/circomlib/circuits/gates.circom";
include "node_modules/circomlib/circuits/bitify.circom";
include "node_modules/circomlib/circuits/comparators.circom";
include "node_modules/circomlib/circuits/binsum.circom";
include "node_modules/circomlib/circuits/multiplexer.circom";

template IfElse() {
  signal input cond;
  signal input L;
  signal input R;
  signal output out;

  out <== cond * (L - R) + R;
}

template Rotate() {
    signal input in;
    signal input idx;

    signal output out[32];
    signal output decimalOut;
    
    signal rotate_amount[64] <== [
        7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
        5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20,
	    4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
	    6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21
    ];

    // Based on the idx, we need to choose the correct rotator from the array rotate_amount
    signal index_rotator;
    component multiplexer = Multiplexer(1, 64);
    var arr[64][1];
    for(var i = 0; i < 64; i++) {
        arr[i][0] = rotate_amount[i];
    }

    multiplexer.inp <== arr;
    multiplexer.sel <== idx;

    index_rotator <== multiplexer.out[0];

    // Now we rotate the input in by rotate_amount[index_rotator]
    signal og_number[32];
    signal rotated_number[32];

    component n2b = Num2Bits(32);
    n2b.in <== in;

    og_number <== n2b.out;

    signal rot_num_idx[32];
    signal cond1[32];
    signal cond2[32];
    component isGreaterEqThan[32];
    for(var i = 0; i < 32; i++) {
        isGreaterEqThan[i] = GreaterEqThan(32);

        isGreaterEqThan[i].in[0] <== i + index_rotator;
        isGreaterEqThan[i].in[1] <== 32;

        cond1[i] <== (index_rotator + i - 32) * isGreaterEqThan[i].out;
        cond2[i] <== (index_rotator + i) * (1 - isGreaterEqThan[i].out);

        rot_num_idx[i] <== cond1[i] + cond2[i];
    }

    component quinSelector[32];
    var array[32][1];
    for(var i = 0; i < 32; i++) {
        array[i][0] = og_number[i];
    }

    for(var i = 0; i < 32; i++) {
        quinSelector[i] = Multiplexer(1, 32);

        quinSelector[i].inp <== array;
        quinSelector[i].sel <== 31 - rot_num_idx[i];

        rotated_number[31 - i] <== quinSelector[i].out[0];
    }

    component b2n = Bits2Num(32);
    b2n.in <== rotated_number;
    decimalOut <== b2n.out;
    out <== rotated_number;
}

template AddConstantFromK() {

    signal input in;
    signal input idx;

    signal output out[32];
    signal output decimalOut;

    /*
        signal K[64] <== [
            0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee
            0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
            0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be, 
            0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821, 
            0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa, 
            0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8, 
            0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed, 
            0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a, 
            0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c, 
            0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70, 
            0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05, 
            0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665, 
            0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039, 
            0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1, 
            0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1, 
            0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391 
        ];
    */
    signal K[64] <== [
        3614090360, 3905402710, 606105819, 3250441966,    
        4118548399, 1200080426, 2821735955, 4249261313,    
        1735328473, 2368359562, 4294925233, 2336552878,    
        1755183732, 4243563511, 2850285822, 1236535329,    
        4118548394, 3225465664, 643717713, 3921069994,    
        3600352805, 38016083, 4096336452, 3875764656,    
        568446438, 3275163606, 4107603335, 1163531501,    
        2850285829, 4249261313, 1735328473, 2368359562,   
        4294925234, 2272392833, 1839030562, 4279200364,    
        2763975236, 1272893353, 4149444224, 3183894230,    
        681279174, 3936430074, 3572445317, 761622411,    
        3654602809, 3866312157, 531131761, 3302518473,    
        4096336452, 113926993, 2889905487, 4237533241,    
        1700485571, 2399980690, 4293915773, 2240044497,    
        1873313359, 4264355552, 2600822928, 1313534501,    
        4157632342, 3174756917, 719683457, 3951481745
    ];


    // Based on the idx, we need to choose the correct constant from the array K
    signal index_constant;
    component multiplexer = Multiplexer(1, 64);
    var arr[64][1];
    for(var i = 0; i < 64; i++) {
        arr[i][0] = K[i];
    }

    multiplexer.inp <== arr;
    multiplexer.sel <== idx;

    index_constant <== multiplexer.out[0];

    component badd = Add32();
    badd.in[0] <== in;
    badd.in[1] <== index_constant;

    out <== badd.out;
    decimalOut <== badd.decimalOut;
}

// template for doing 32 bit addition
template Add32() {
    signal input in[2];
    signal output decimalOut;
    signal output out[32];

    component band = BitwiseAND();

    band.in[0] <== in[0] + in[1];
    band.in[1] <== 4294967295; // (0xFFFFFFFF) This mask ensures that only 32 bit addition happens
    out <== band.out;
    decimalOut <== band.decimalOut;

    // Now we need to place constraints to verify if the calculation is indeed correct or not
    component bsum = BinSum(32, 2);
    component n2b[2];

    n2b[0] = Num2Bits(32);
    n2b[1] = Num2Bits(32);

    n2b[0].in <== in[0];
    n2b[1].in <== in[1];

    bsum.in[0] <== n2b[0].out;
    bsum.in[1] <== n2b[1].out;

    for(var i = 0; i < 32; i++) {
        bsum.out[i] === out[i];
    }
}

template BitwiseAND() {
    signal input in[2];
    signal output out[32];
    signal output decimalOut;

    signal Ab[32];
    signal Bb[32];

    component bitwiseAnd[32];
    component num2Bits[2];

    num2Bits[0] = Num2Bits(32);
    num2Bits[1] = Num2Bits(32);

    num2Bits[0].in <== in[0];
    num2Bits[1].in <== in[1];

    Ab <== num2Bits[0].out;
    Bb <== num2Bits[1].out;

    for(var i = 0; i < 32; i++) {
        bitwiseAnd[i] = AND();

        bitwiseAnd[i].a <== Ab[i];
        bitwiseAnd[i].b <== Bb[i];

        out[i] <== bitwiseAnd[i].out;
    }

    component bits2Num = Bits2Num(32);
    bits2Num.in <== out;
    decimalOut <== bits2Num.out;
}

template BitwiseOR() {
    signal input in[2];
    signal output out[32];
    signal output decimalOut;

    signal Ab[32];
    signal Bb[32];

    component bitwiseOr[32];
    component num2Bits[2];

    num2Bits[0] = Num2Bits(32);
    num2Bits[1] = Num2Bits(32);

    num2Bits[0].in <== in[0];
    num2Bits[1].in <== in[1];

    Ab <== num2Bits[0].out;
    Bb <== num2Bits[1].out;

    for(var i = 0; i < 32; i++) {
        bitwiseOr[i] = OR();

        bitwiseOr[i].a <== Ab[i];
        bitwiseOr[i].b <== Bb[i];

        out[i] <== bitwiseOr[i].out;
    }

    component bits2Num = Bits2Num(32);
    bits2Num.in <== out;
    decimalOut <== bits2Num.out;
}

template BitwiseNOT() {
    signal input in;
    signal output out[32];
    signal output decimalOut;

    signal Ab[32];

    component bitwiseNot[32];
    component num2Bits = Num2Bits(32);

    num2Bits.in <== in;

    Ab <== num2Bits.out;

    for(var i = 0; i < 32; i++) {
        bitwiseNot[i] = NOT();

        bitwiseNot[i].in <== Ab[i];
        out[i] <== bitwiseNot[i].out;
    }

    component bits2Num = Bits2Num(32);
    bits2Num.in <== out;
    decimalOut <== bits2Num.out;
}   

template BitwiseXOR() {
    signal input in[2];
    signal output out[32];
    signal output decimalOut;

    signal Ab[32];
    signal Bb[32];

    component bitwiseXor[32];
    component num2Bits[2];

    num2Bits[0] = Num2Bits(32);
    num2Bits[1] = Num2Bits(32);

    num2Bits[0].in <== in[0];
    num2Bits[1].in <== in[1];

    Ab <== num2Bits[0].out;
    Bb <== num2Bits[1].out;

    for(var i = 0; i < 32; i++) {
        bitwiseXor[i] = XOR();

        bitwiseXor[i].a <== Ab[i];
        bitwiseXor[i].b <== Bb[i];

        out[i] <== bitwiseXor[i].out;
    }

    component bits2Num = Bits2Num(32);
    bits2Num.in <== out;
    decimalOut <== bits2Num.out;
}

template CombineLoop1() {
    //F = (B & C) | (~B & D)

    signal input A, B, C, D;
    signal input in;
    signal input idx;
    signal output out;

    signal exp1;
    signal exp2;
    signal exp3;

    signal F;
    signal FPlusA;
    signal FPlusAPlusIn;
    signal FPlusAPlusInPlusK;
    signal RotatedFPlusAPlusInPlusK;

    component band[2];

    component bnot = BitwiseNOT();
    band[0] = BitwiseAND();
    band[1] = BitwiseAND();
    component bor = BitwiseOR();

    bnot.in <== B;
    exp1 <== bnot.decimalOut;
    
    band[0].in[0] <== exp1;
    band[0].in[1] <== D;

    exp2 <== band[0].decimalOut;

    band[1].in[0] <== B;
    band[1].in[1] <== C;

    exp3 <== band[1].decimalOut;

    bor.in[0] <== exp2;
    bor.in[1] <== exp3;

    F <== bor.decimalOut;

    // Now we need to do a 32 bit addition of F with the A
    component badd32[3];

    badd32[0] = Add32();
    badd32[1] = Add32();
    badd32[2] = Add32();

    badd32[0].in[0] <== F;
    badd32[0].in[1] <== A;

    FPlusA <== badd32[0].decimalOut;

    // Now we need to do a 32 bit addition of FplusA with input segment of that current iteration
    badd32[1].in[0] <== FPlusA;
    badd32[1].in[1] <== in;
    
    FPlusAPlusIn <== badd32[1].decimalOut;

    // Now we need to add the corresponding constant to the earlier result
    component addK = AddConstantFromK();
    addK.in <== FPlusAPlusIn;
    addK.idx <== idx;

    FPlusAPlusInPlusK <== addK.decimalOut;

    // Now we need to rotate FPlusAPlusInPlusK
    component rotatoor = Rotate();
    rotatoor.in <== FPlusAPlusInPlusK;
    rotatoor.idx <== idx;

    RotatedFPlusAPlusInPlusK <== rotatoor.decimalOut;

    // Finally we will add B to the above result again
    badd32[2].in[0] <== RotatedFPlusAPlusInPlusK;
    badd32[2].in[1] <== B;

    out <== badd32[2].decimalOut;
}

template CombineLoop2() {
    // F = (B & D) | (C & ~D)
    signal input A, B, C, D;
    signal input idx;
    signal input in;
    signal output out;

    signal exp1;
    signal exp2;
    signal exp3;

    signal F;
    signal FPlusA;
    signal FPlusAPlusIn;
    signal FPlusAPlusInPlusK;
    signal RotatedFPlusAPlusInPlusK;

    component band[2];

    component bnot = BitwiseNOT();
    band[0] = BitwiseAND();
    band[1] = BitwiseAND();
    component bor = BitwiseOR();

    bnot.in <== D;
    exp1 <== bnot.decimalOut;
    
    band[0].in[0] <== exp1;
    band[0].in[1] <== C;

    exp2 <== band[0].decimalOut;

    band[1].in[0] <== B;
    band[1].in[1] <== D;

    exp3 <== band[1].decimalOut;

    bor.in[0] <== exp2;
    bor.in[1] <== exp3;

    F <== bor.decimalOut;

    // Now we need to do a 32 bit addition of F with the A
    component badd32[3];

    badd32[0] = Add32();
    badd32[1] = Add32();
    badd32[2] = Add32();

    badd32[0].in[0] <== F;
    badd32[0].in[1] <== A;

    FPlusA <== badd32[0].decimalOut;

    // Now we need to do a 32 bit addition of FplusA with input segment of that current iteration
    badd32[1].in[0] <== FPlusA;
    badd32[1].in[1] <== in;
    
    FPlusAPlusIn <== badd32[1].decimalOut;

    // Now we need to add the corresponding constant to the earlier result
    component addK = AddConstantFromK();
    addK.in <== FPlusAPlusIn;
    addK.idx <== idx;

    FPlusAPlusInPlusK <== addK.decimalOut;

    // Now we need to rotate FPlusAPlusInPlusK
    component rotatoor = Rotate();
    rotatoor.in <== FPlusAPlusInPlusK;
    rotatoor.idx <== idx;

    RotatedFPlusAPlusInPlusK <== rotatoor.decimalOut;

    // Finally we will add B to the above result again
    badd32[2].in[0] <== RotatedFPlusAPlusInPlusK;
    badd32[2].in[1] <== B;

    out <== badd32[2].decimalOut;
}

template CombineLoop3() {
    // F = B ^ C ^ D
    signal input A, B, C, D;
    signal input idx;
    signal input in;
    signal output out;

    signal exp1;
    signal F;
    signal FPlusA;
    signal FPlusAPlusIn;
    signal FPlusAPlusInPlusK;
    signal RotatedFPlusAPlusInPlusK;

    component bxor[2];

    bxor[0] = BitwiseXOR();
    bxor[1] = BitwiseXOR();

    bxor[0].in[0] <== D;
    bxor[0].in[1] <== C;

    exp1 <== bxor[0].decimalOut;

    bxor[1].in[0] <== exp1;
    bxor[1].in[1] <== B;

    F <== bxor[1].decimalOut;

    // Now we need to do a 32 bit addition of F with the A
    component badd32[3];

    badd32[0] = Add32();
    badd32[1] = Add32();
    badd32[2] = Add32();

    badd32[0].in[0] <== F;
    badd32[0].in[1] <== A;

    FPlusA <== badd32[0].decimalOut;

    // Now we need to do a 32 bit addition of FplusA with input segment of that current iteration
    badd32[1].in[0] <== FPlusA;
    badd32[1].in[1] <== in;
    
    FPlusAPlusIn <== badd32[1].decimalOut;

    // Now we need to add the corresponding constant to the earlier result
    component addK = AddConstantFromK();
    addK.in <== FPlusAPlusIn;
    addK.idx <== idx;

    FPlusAPlusInPlusK <== addK.decimalOut;

    // Now we need to rotate FPlusAPlusInPlusK
    component rotatoor = Rotate();
    rotatoor.in <== FPlusAPlusInPlusK;
    rotatoor.idx <== idx;

    RotatedFPlusAPlusInPlusK <== rotatoor.decimalOut;

    // Finally we will add B to the above result again
    badd32[2].in[0] <== RotatedFPlusAPlusInPlusK;
    badd32[2].in[1] <== B;

    out <== badd32[2].decimalOut;
}

template CombineLoop4() {
    // F = C ^ (B | ~ D)
    signal input A, B, C, D;
    signal input in;
    signal input idx;
    signal output out;

    signal exp1;
    signal exp2;
    signal F;
    signal FPlusA;
    signal FPlusAPlusIn;
    signal FPlusAPlusInPlusK;
    signal RotatedFPlusAPlusInPlusK;

    component bnot = BitwiseNOT();
    component bor = BitwiseOR();
    component bxor = BitwiseXOR();

    bnot.in <== D;
    exp1 <== bnot.decimalOut;

    bor.in[0] <== exp1;
    bor.in[1] <== B;
    exp2 <== bor.decimalOut;

    bxor.in[0] <== exp2;
    bxor.in[1] <== C;

    F <== bxor.decimalOut;

    // Now we need to do a 32 bit addition of F with the A
    component badd32[3];

    badd32[0] = Add32();
    badd32[1] = Add32();
    badd32[2] = Add32();

    badd32[0].in[0] <== F;
    badd32[0].in[1] <== A;

    FPlusA <== badd32[0].decimalOut;

    // Now we need to do a 32 bit addition of FplusA with input segment of that current iteration
    badd32[1].in[0] <== FPlusA;
    badd32[1].in[1] <== in;
    
    FPlusAPlusIn <== badd32[1].decimalOut;

    // Now we need to add the corresponding constant to the earlier result
    component addK = AddConstantFromK();
    addK.in <== FPlusAPlusIn;
    addK.idx <== idx;

    FPlusAPlusInPlusK <== addK.decimalOut;

    // Now we need to rotate FPlusAPlusInPlusK
    component rotatoor = Rotate();
    rotatoor.in <== FPlusAPlusInPlusK;
    rotatoor.idx <== idx;

    RotatedFPlusAPlusInPlusK <== rotatoor.decimalOut;

    // Finally we will add B to the above result again
    badd32[2].in[0] <== RotatedFPlusAPlusInPlusK;
    badd32[2].in[1] <== B;

    out <== badd32[2].decimalOut;
}

template Md5() {
    // since the word size in the MD5 algo is 32 bits and the input size is 512, let's create an input signal of 16 elements.
    signal input in[16];
    signal input out[4]; // The hash result

    ////////////////////////////////
    /// Range Check on the input in
    ////////////////////////////////
    // We need to constrain the input signals to be less than 2**32.
    component isLessEqThan[16];
    signal rangeCheckArray[16];

    for(var i = 0; i < 16; i++) {
        isLessEqThan[i] = LessEqThan(32);

        isLessEqThan[i].in[0] <== 0;
        isLessEqThan[i].in[1] <== ((in[i] - 0)*(2**32 - in[i]));

        rangeCheckArray[i] <== isLessEqThan[i].out;
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

    component combine1[16];
    component combine2[16];
    component combine3[16];
    component combine4[16];

    // First loop of state changes
    for(var i = 1; i < 16; i++) {
        combine1[i] = CombineLoop1();

        combine1[i].A <== states[i-1][0];
        combine1[i].B <== states[i-1][1];
        combine1[i].C <== states[i-1][2];
        combine1[i].D <== states[i-1][3];
        combine1[i].in <== in[i];
        combine1[i].idx <== i;

        states[i][0] <== states[i-1][3];
        states[i][1] <== combine1[i].out;
        states[i][2] <== states[i-1][1];
        states[i][3] <== states[i-1][2];
    }

    // Second loop of state changes
    var c2_idx = 0;
    for(var j = 16; j < 32; j++) {
        combine2[c2_idx] = CombineLoop2();

        combine2[c2_idx].A <== states[j-1][0];
        combine2[c2_idx].B <== states[j-1][1];
        combine2[c2_idx].C <== states[j-1][2];
        combine2[c2_idx].D <== states[j-1][3];
        combine2[c2_idx].in <== in[(5 * j + 1) % 16];
        combine2[c2_idx].idx <== j;


        states[j][0] <== states[j-1][3];
        states[j][1] <== combine2[c2_idx].out;
        states[j][2] <== states[j-1][1];
        states[j][3] <== states[j-1][2];

        c2_idx++;
    }

    // Third loop of state changes
    var c3_idx = 0;
    for(var l = 32; l < 48; l++) {
        combine3[c3_idx] = CombineLoop3();

        combine3[c3_idx].A <== states[l-1][0];
        combine3[c3_idx].B <== states[l-1][1];
        combine3[c3_idx].C <== states[l-1][2];
        combine3[c3_idx].D <== states[l-1][3];
        combine3[c3_idx].in <== in[(3 * l + 5) % 16];
        combine3[c3_idx].idx <== l;

        states[l][0] <== states[l-1][3];
        states[l][1] <== combine3[c3_idx].out;
        states[l][2] <== states[l-1][1];
        states[l][3] <== states[l-1][2];

        c3_idx++;
    }

    // Fourth loop of state changes
    var c4_idx = 0;
    for(var k = 48; k < 64; k++) {
        combine4[c4_idx] = CombineLoop4();

        combine4[c4_idx].A <== states[k-1][0];
        combine4[c4_idx].B <== states[k-1][1];
        combine4[c4_idx].C <== states[k-1][2];
        combine4[c4_idx].D <== states[k-1][3];
        combine4[c4_idx].in <== in[(7 * k) % 16];
        combine4[c4_idx].idx <== k;

        states[k][0] <== states[k-1][3];
        states[k][1] <== combine4[c4_idx].out;
        states[k][2] <== states[k-1][1];
        states[k][3] <== states[k-1][2];

        c4_idx++;
    }

    // Making sure that the last state change corresponds to the hash output inputted to the circuit
    out[0] === states[63][0];
    out[1] === states[63][1];
    out[2] === states[63][2];
    out[3] === states[63][3];
}

component main{ public [ in, out ] } = Md5();