--  Based on regex example from Rosettacode

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Gnat.Regpat; use Gnat.Regpat;

with ML_Types;

procedure Regex_Example is

    procedure Find_Match
      (Compiled_Expression : Pattern_Matcher; Text : String;
       First, Last : out Positive; Found : out Boolean) is
        Result : Match_Array (0 .. 1);
    begin
        --  Match selects the first substring of Text that matches
        --  Compiled_Expression
        Match (Compiled_Expression, Text, Result);
        Found := Result (1) /= No_Match;

        if Found then
            First := Result (1).First;
            Last := Result (1).Last;
        end if;

   end Find_Match;

   --  -------------------------------------------------------------------------

    use ML_Types.String_Package;
    --  [a-zA-Z]+ selects a sequence [] of one or more (+)
    --  alphabetic characters (a-zA-Z)
    --  () combines [a-zA-Z]+ into a group
    Word_Pattern  : constant String := "([a-zA-Z]+)";
    Matcher       : constant Pattern_Matcher := Compile (Word_Pattern);
    Str           : String := "I love PATTERN matching!";
    Current_First : Positive := Str'First;
    First         : Positive;
    Last          : Positive;
    Words         : ML_Types.String_List;
    Curs          : Cursor;
    Found         : Boolean := True;
begin
    --  first, find all the words in Str
    while Found loop
        Find_Match (Matcher, Str (Current_First .. Str'Last),
                    First, Last, Found);
        if Found then
            Words.Append (To_Unbounded_String (Str (First .. Last)));
            Put_Line ("<" & Str (First .. Last) & ">");
        end if;
        Current_First := Last + 1;
    end loop;

    --  second, replace "PATTERN" in Str by "pattern"
    Find_Match (Compile ("(PATTERN)"), Str, First, Last, Found);
    pragma Unreferenced (Found);

    Str :=
      Str (Str'First .. First - 1) & "pattern" & Str (Last + 1 .. Str'Last);
    Put_Line (Str);

    New_Line;
    Curs := Words.First;
    while Has_Element (Curs) loop
        Put_Line ("<<" & Element (Curs) & ">>");
        Next (Curs);
    end loop;

end Regex_Example;
