--  Regex example from Rosettacode

with Ada.Text_IO; use Ada.Text_IO;
with Gnat.Regpat; use Gnat.Regpat;

procedure Use_Regex is

    procedure Search_For_Pattern
      (Compiled_Expression : Gnat.Regpat.Pattern_Matcher;
       Search_In : String; First, Last : out Positive; Found : out Boolean) is
        Result : Match_Array (0 .. 1);
    begin
        Match (Compiled_Expression, Search_In, Result);
        Found := not Gnat.Regpat."=" (Result (1), No_Match);
        if Found then
            First := Result (1).First;
            Last := Result (1).Last;
        end if;

    end Search_For_Pattern;

    Word_Pattern : constant String := "([a-zA-Z]+)";

    Str :           String := "I love PATTERN matching!";
    Current_First : Positive := Str'First;
    First, Last :   Positive;
    Found :         Boolean;

begin
    --  first, find all the words in Str
    loop
        Search_For_Pattern (Compile (Word_Pattern),
                            Str (Current_First .. Str'Last),
                            First, Last, Found);
        exit when not Found;
        Put_Line ("<" & Str (First .. Last) & ">");
        Current_First := Last + 1;
    end loop;

    --  second, replace "PATTERN" in Str by "pattern"
    Search_For_Pattern (Compile ("(PATTERN)"), Str, First, Last, Found);
    pragma Unreferenced (Found);
    Str := Str (Str'First .. First - 1) & "pattern" & Str (Last + 1 .. Str'Last);
    Put_Line (Str);

end Use_Regex;
