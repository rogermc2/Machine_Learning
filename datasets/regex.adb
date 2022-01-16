
package body Regex is

   procedure Find_Match
      (Compiled_Expression : Gnat.Regpat.Pattern_Matcher; Text : String;
       First, Last : out Positive; Found : out Boolean) is
      use Gnat.Regpat;
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

end Regex;
