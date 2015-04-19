// c# program that proves the halting theorem
// i.e. there can be no computer program  which tells you 
// if another computer program loops forever or not.
// Assume there is such a procedure which works for any procedure p and argument a:
// bool halts(Action<object> p, object a)
//{
//  magic stuff...
//}
// we could then write:

void run-forever()
{
  while(true);
}

void try(Action<object> p)
{
  if (halts(p, p))
  {
    // doesn't halt
    run-forever();
  }
  else
  {
    // halts
    return;
  }
}

// And then call try(try)...  what should halts(try try) return??
// Is it possible to write this with generics?? (i.e. replace object with T)??