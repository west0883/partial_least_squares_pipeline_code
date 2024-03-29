% PopulateResponseVariables.m
% Sarah West
% 6/6/22

% Run with RunAnalysis, on each mouse. Not doing anything fancy with the
% input loops because this is a very specific use-case function.

function [parameters] = PopulateResponseVariables(parameters)

    MessageToUser('Populating ', parameters);

    % If vertically concatenating, make a new empty response variable cell array
    if isfield(parameters, 'concatenate_vertically') && parameters.concatenate_vertically
        response_variables = cell(size(parameters.periods, 1), 1);
    else
        % Otherwise, 
        % Make a new table out of periods.
        response_variables = parameters.periods; 

        % Make a holder cell that has a column each response variable.
        response_variables_cellholder = cell(size(parameters.periods, 1), numel(parameters.response_variable_names));
    end 

    % For each period
    for periodi = 1:size(parameters.periods, 1)

        % Get number of instances 
        instances = size(parameters.data{periodi}, 3);

        % For each of the static categories (static across different
        % instances: motorized_vs_spon, type, duration, transition or not
        for statici = 1:numel(parameters.variables_static)

           % Replicate by number of instancs in 3rd dimension.
           response_variables_structure.(parameters.variables_static{statici}) = repmat(parameters.periods{periodi, parameters.variables_static{statici}}{1}, 1, 1, instances);

        end

        % ***Put in pupil diameters

        % Use original location in periods_nametable via index field.
        index = parameters.periods{periodi, 'index'};
        
        % Get out the diameters
        diameters = parameters.diameter_vector{index};

        % Permute diameters so dimensions match other response variables (put in a 1 at the beginning for the number of variables)
        diameters = permute(diameters, [3, 1, 2]);  % Use "3" as the first dimension, because it's a 1.

        % Put into structure.
        response_variables_structure.pupil_diameter_vector = diameters;

        % Skip these steps if there's no motorized_variables_static field,
        % just replicate the speed & accel vectors of all periods.
        if isfield(parameters, 'motorized_variables_static')
        
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

        % If no motorized static, just replicate all speeds & accels
        else
            variables = {'speed_vector', 'accel_vector'};
            for variablei = 1:numel(variables)
                variable = variables{variablei};
                
                % Replicate by number of instancs in 3rd dimension.
                response_variables_structure.(variable) = repmat(parameters.periods{periodi, variable}{1}, 1, 1, instances);
                                                          
            end 
        end

        % Put variables into a cell format, 
       
        % get all variable names you've used.
         fieldnames(response_variables_structure)

        cell_holder = cell(1,numel(fieldnames(response_variables_structure))); 
        for variablei = 1:numel(fieldnames(response_variables_structure)) 
    
            cell_holder{variablei} = response_variables_structure.(parameters.response_variable_names{variablei});
    
        end 

        % Concatenate vertically (dimension 1) so all variables are together, like with the correlations. 
        if isfield(parameters, 'concatenate_vertically') && parameters.concatenate_vertically
        
            % Vertically concatenate holder cells.
            response_variables{periodi} = vertcat(cell_holder{:});

        else
            % Put into the response variable cell holder
            for variablei = 1:numel(parameters.response_variable_names)
                response_variables_cellholder{periodi, variablei} = cell_holder{variablei};
            end

        end

    % If not vertically concatenated, make each column of
    % response_variables_cellholder its own variable in the
    % response_variables table. 
    if (~isfield(parameters, 'concatenate_vertically')) || (isfield(parameters, 'concatenate_vertically') & ~parameters.concatenate_vertically)
       
        % Put into the response variable cell holder
        for variablei = 1:numel(parameters.response_variable_names)
            response_variables.(parameters.response_variable_names{variablei}) =  response_variables_cellholder(:,variablei);
        end

    end
    % Pass response variables to output structure
    parameters.response_variables = response_variables;

end
 
