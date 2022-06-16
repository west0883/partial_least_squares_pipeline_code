
% p = number of components to calculate for;
function [broken_stick_line] = BrokenStickModel(p)
    
    broken_stick_line = NaN(p,1);
    % For each component,
    for i = 1:p

        broken_stick_line(i) = sum(1 ./ [i:p])./p;
 
    end
   
end