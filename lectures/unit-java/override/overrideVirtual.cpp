#include <iostream>
using namespace std;

class Base
{
	public:
	virtual int f() { return 1; }
};

class Derived: public Base
{
	public:
	int f() { return 2; }
};

int main()
{
	Base b;
	Derived d;

	cout << b.f() << endl;
	cout << d.f() << endl;
	b = d;
	cout << b.f() << endl;

	Base *b2 = &d;
	cout << b2->f() << endl;
}
