
package body Export_Types is

    package body Elements is

        function "=" (Left, Right : Element) return Boolean is
            use Attribute_Maps;
        begin
            return Right.Source = Left.Source and then
              Right.Target = Left.Target and then
              Right.Attributes = Left.Attributes;
        end "=";

    end Elements;

end Export_Types;
