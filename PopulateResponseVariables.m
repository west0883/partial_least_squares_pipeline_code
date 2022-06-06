% PopulateResponseVariables.m
% Sarah West
% 6/6/22

% Run with RunAnalysis, on each mouse. Not doing anything fancy with the
% input loops because this is a very specific use-case function.

function [parameters] = PopulateResponseVariables(parameters)

    MessageToUser('Populating ', parameters);

    % Make a new empty response variable cell array
    response_variables = cell(size(parameters.periods, 1), 1);

    % For each period
    for periodi = 1:size(parameters.periods, 1)

        % Get number of instances 
        instances = size(parameters.data{periodi}, 3);

        % For each of the static categories (static across different
        % instances: motorized_vs_spon, type, duration
        for statici = 1:numel(parameters.variables_static)

           % Replicate by number of instancs in 3rd dimension.
           response_variables_structure.(parameters.variables_static{statici}) = repmat(parameters.periods{periodi, parameters.variables_static{statici}}{1}, 1, 1, instances);

        end

        % If this period is motorized, 
        if strcmp(parameters.periods{periodi, 'motorized_vs_spon'}{1}, 'motorized')
            % For each of the motorized-specific static categories.
            for statici = 1:numel(parameters.motorized_variables_static)
            
                % Replicate by number of instancs in 3rd dimension.
                response_variables_structure.(parameters.motorized_variables_static{statici}) = repmat(parameters.periods{periodi, parameters.motorized_variables_static{statici}}{1}, 1, 1, instances);
            end 
    
        % Else, this period is spontaneous 
        else 
            % Get the condition name 
            condition = parameters.periods{periodi, 'condition'}{1};

            % Find location in the speed & accel vectors
            location = logical(cellfun(@strcmp, parameters.spontaneous_periods_order', ...
                repmat({condition},numel(parameters.spontaneous_periods_order), 1))); 

            % For each of the motorized-specific static categories.
            for statici = 1:numel(parameters.motorized_variables_static)

               % Get out the needed values.
               values = parameters.(parameters.motorized_variables_static{statici}){location};

               % Take absolute value. (So you don't get negative speeds or
               % accels. I don't want 0 to be in the middle of the curve).
               values = abs(values);

               % Permute the values to make it match the format of all the
               % other response variables. (put in a 1 at the beginning for the number of variables)
               values = permute(values, [3, 1, 2]);  % Use "3" as the first dimension, because it's a 1.

               % Check if dim 3 == instances

               % Put into structure.
               response_variables_structure.(parameters.motorized_variables_static{statici}) = values;
            
            end 
        end

        % Concatenate vertically (dimension 1) so all variables are together, like with the correlations.

        % Put variables into a cell format, 
        cell_holder = cell(numel(parameters.response_variable_names),1); 
        for variablei = 1:numel(parameters.response_variable_names) 

            cell_holder{variablei} = response_variables_structure.(parameters.response_variable_names{variablei});

        end 
    
        % Vertically concatenate holder cells.
        response_variables{periodi} = vertcat(cell_holder{:});

    end

    % Pass response variables to output structure
    parameters.response_variables = response_variables;

end
 
