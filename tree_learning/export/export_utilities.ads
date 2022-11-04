
with Config;
with Export_Types; use Export_Types;
with Lines;

package Export_Utilities is

   function Arg_Max (Values : Colours_List) return Positive;
   procedure Get_Name (aLine   : in out Lines.Bounded_String;
                       Finish  : in out Natural;
                       N       : out Config.Name);
   function Pad (S : in String) return Config.Name;

end Export_Utilities;
