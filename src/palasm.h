#ifndef _PALASM_H__
#define _PALASM_H__


class Palasm {
public:
    Palasm();

    virtual ~Palasm() = default;
    void sumchk();
    void plotf();
    void tweek(int chipType);
    
protected:
    int _ipt;
    bool _lblank, _lleft, _land, _lor,
         _lslash, _lequal, _lright,
         _lxor, _lxnor;
    int _ipage[80][200]; //PGE
    bool _lfuses[32][64],_lphant[32][64]; //LFUSES
    int _ifunct,_idesc,_iend; //FTEST
    char _bufio[32], _idec[4], _iot;
    int _isum[4];
};

#endif //_PALASM_H__
