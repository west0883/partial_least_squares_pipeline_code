% PlotBetasSecondLevel.m
% Sarah West 
% 6/26/22

% Plot a number of Betas from partial least squares regression. Run by RunAnalysis.

function [parameters] = PlotBetasSecondLevel(parameters)

    % Some things need to be done differently with the continuous vs
    % categorical comparisons. (Separate out each continuous variable)

    % Get number of subplot to use.
    % Just use the total number of comparisons.
    [subplot_rows, subplot_columns] = OptimizeSubplotNumbers(size(parameters.this_comparison_set, 2),4/5);

    % If using averaging,
    if isfield(parameters, 'averaging_across_mice') && parameters.averaging_across_mice

        betas = parameters.average_across_mice;

    else
        % If using 2nd-level PLSR
        % Pull out betas to use going forward (using just the intercepts).
        betas = parameters.results.BETA(1, :);

    end
   
    % Adjust Betas based on z-score sigma. % First row is constant estimate
    % If user says so
    if isfield(parameters, 'adjustBetas') && parameters.adjustBetas
        % if categorical, only use first set of sigmas.
        if strcmp(parameters.comparison_type, 'categorical')
            betas_adjusted = betas .* parameters.average_sigmas(1:numel(betas));
        else
            betas_adjusted = betas .* parameters.average_sigmas;
        end
    else
        betas_adjusted = betas;
    end

    % If user wants to use significance for plotting,
    if isfield(parameters, 'useSignificance') && parameters.useSignificance

        % If significance matrix doesn't match in size, transpose it.
        if ~isequal(size(betas_adjusted), size(parameters.significance))

            significance = parameters.significance';
        else
            significance = parameters.significance;
        end

        % Only keep/plot beta values that reach significance 
        betas_adjusted = betas_adjusted .* significance; 

    end
    
    % Get comparison iterator for subplot location. 
    iterator_location = strcmp(parameters.keywords, {'comparison_iterator'});
    comparison_iterator = parameters.values{iterator_location};

    % Get comparison name for subplot title. 
    name_location = strcmp(parameters.keywords, {'comparison'});
    comparison = parameters.values{name_location};

    % Make a colormap with cbrewer; 
    cmap= flipud(cbrewer('div', 'RdBu', 512, 'linear'));

    % Start plotting. 

    % If NOT plotting individually,
    if ~isfield(parameters, 'plotIndividually') || ~parameters.plotIndividually
    
        % If categorical, 
        if strcmp(parameters.comparison_type, 'categorical')
    
            % If there isn't a figure for this yet, make one.
            if ~isfield(parameters, 'fig')
                fig = figure;
                fig.WindowState = 'maximized';
    
                % Make title.
                title_string = ['Betas, second level ' parameters.comparison_type];
                if isfield(parameters, 'adjustBetas') && parameters.adjustBetas
                    title_string = [title_string ', sigma adjusted'];
                end
                if isfield(parameters, 'useSignificance') && parameters.useSignificance
                    title_string = [title_string ', significant only'];
                end
                title_string = strrep(title_string, '_', ' ');
                sgtitle(title_string);
    
                % Put into output structure.
                parameters.fig = fig;
            end
    
            % Just need one plot per comparison.
            holder = NaN(parameters.number_of_sources, parameters.number_of_sources);
            holder(parameters.indices) = betas_adjusted;
        
            % Check if this comparison needs to be multiplied by -1 or not.
            

            
            % If adjusted, use a standard color range for each plot.
            if isfield(parameters, 'adjustBetas') && parameters.adjustBetas
                color_range = parameters.color_range;
            % If not adjusted, make a fitted color range for this plot.
            else
                extreme = max(max(holder, [], 'all', 'omitnan'), abs(min(holder, [], 'all', 'omitnan')));
                color_range = [-extreme extreme]; 
            end
    
            % Plot.
            subplot(subplot_rows, subplot_columns, comparison_iterator); imagesc(holder); 
            colormap(cmap); colorbar; caxis(color_range);
        
            % Make subplot title.
            title_string = erase(comparison, parameters.comparison_type); 
            title(strrep(title_string, '_', ' ')); axis square;
    
        % If continuous, 
        else
            % Make figures for each variable type.
            for variablei = 1:numel(parameters.continuous_variable_names)
                variable = parameters.continuous_variable_names{variablei};
    
                if ~isfield(parameters, [variable '_fig'])
                    figure_holder = figure;
                    figure_holder.WindowState = 'maximized';
                    
                    % Make title.
                    title_string = ['Betas, ' variable ' second level ' parameters.comparison_type];
                    if isfield(parameters, 'adjustBetas') && parameters.adjustBetas
                        title_string = [title_string ', sigma adjusted'];
                    end
                    if isfield(parameters, 'useSignificance') && parameters.useSignificance
                        title_string = [title_string ', significant only'];
                    end
                    title_string = strrep(title_string, '_', ' ');
                    sgtitle(title_string);
    
                    % Put into output structure.
                    parameters.([variable '_fig']) = figure_holder;
    
                end
            end
    
            % Figure out where each beta matrix needs to go. 
            
            % Use the comparisons matrix to get the variables you're using for
            % this comparison.
            variablesToUse = parameters.this_comparison_set(comparison_iterator).variablesToUse;
    
            % Make a holder matrix for separated variable betas
            betas_separated_variables = NaN(parameters.number_of_sources, parameters.number_of_sources, numel(variablesToUse));
         
            % Separate out each beta matrix per variable
            for variablei = 1:numel(variablesToUse)
    
                % Make a 2D holder (because lists of indices are weird).
                holder = NaN(parameters.number_of_sources, parameters.number_of_sources);
                
                % Separate
                holder(parameters.indices) = betas_adjusted((variablei - 1) * numel(parameters.indices) + [1:numel(parameters.indices)]);
    
                % Put into 3D holder.
                betas_separated_variables(:,:, variablei) = holder; 
    
            end
    
            % Plot each beta matrix depending on where the variable belongs.
            for variablei = 1:numel(variablesToUse)
    
                % Get the variable name
                variable = erase(variablesToUse{variablei}, '_vector');
    
                % Set the current figure to the one you're interested in
                set(0, 'CurrentFigure', parameters.([variable '_fig']));
    
                % Find its place in the order of variables in continuous_variables_names
                %variable_location = find(strcmp(parameters.continuous_variable_names, {variable}));
            
                % If adjusted, use a standard color range for each plot.
                if isfield(parameters, 'adjustBetas') && parameters.adjustBetas && isfield(parameters, 'useColorRange') && parameters.useColorRange
                    color_range = parameters.color_range;
                % If not adjusted, make a fitted color range for this plot.
                else
                    extreme = max(max(betas_separated_variables(:,:, variablei), [], 'all', 'omitnan'), abs(min(betas_separated_variables(:,:, variablei), [], 'all', 'omitnan')));
                    color_range = [-extreme extreme]; 
                end
                
                % Get subplot index.
                %subplot_index = sub2ind([subplot_rows subplot_columns], variable_location, comparison_iterator);
    
                % Plot
                subplot(subplot_rows, subplot_columns, comparison_iterator); imagesc(betas_separated_variables(:,:, variablei)); 
                colormap(cmap); colorbar; 
                
                % Don't use the color range if both values are 0.
                if any(color_range)
                    caxis(color_range);
                end
               
                % Make subplot title.
                title_string = [erase(comparison, parameters.comparison_type)]; 
                title(strrep(title_string, '_', ' '));  axis square;
    
            end
        end

    

    % Plot individually
    else

         % If categorical, 
        if strcmp(parameters.comparison_type, 'categorical')

            % Duplicate betas across diagonal.


            % Make diagonal = 0;


            % Check if this comparison needs to be multiplied by -1 or not.


           
            % Make a figure
            fig = figure;

            % If adjusted, use a standard color range for each plot.
            if isfield(parameters, 'adjustBetas') && parameters.adjustBetas && isfield(parameters, 'useColorRange') && parameters.useColorRange
                color_range = parameters.color_range;
            % If not adjusted, make a fitted color range for this plot.
            else
                extreme = max(max(betas_separated_variables(:,:, variablei), [], 'all', 'omitnan'), abs(min(betas_separated_variables(:,:, variablei), [], 'all', 'omitnan')));
                color_range = [-extreme extreme]; 
            end


            % Plot 

            % Make a title.

            % Put figure into parameters output structure.
            parameters.fig = fig;
    
        % If continuous, 
        else

            % Use the comparisons matrix to get the variables you're using for
            % this comparison.
            variablesToUse = parameters.this_comparison_set(comparison_iterator).variablesToUse;

            % Separate out each beta matrix per variable
            for variablei = 1:numel(variablesToUse)

                % Get the variable name
                variable = erase(variablesToUse{variablei}, '_vector');
    
                % Make a 2D holder (because lists of indices are weird).
                betas_separated = NaN(parameters.number_of_sources, parameters.number_of_sources);
                
                % Separate
                betas_separated(parameters.indices) = betas_adjusted((variablei - 1) * numel(parameters.indices) + [1:numel(parameters.indices)]);
                
                % Duplicate betas across diagonal

                % Make diagonal blank = 0


                % Make a figure for this variable.
                fig = figure;
                
                % If adjusted, use a standard color range for each plot.
                if isfield(parameters, 'adjustBetas') && parameters.adjustBetas && isfield(parameters, 'useColorRange') && parameters.useColorRange
                    color_range = parameters.color_range;
                % If not adjusted, make a fitted color range for this plot.
                else
                    extreme = max(max(betas_separated_variables(:,:, variablei), [], 'all', 'omitnan'), abs(min(betas_separated_variables(:,:, variablei), [], 'all', 'omitnan')));
                    color_range = [-extreme extreme]; 
                end


                % Plot 

                % Make a title


                % Rename figure handle, put into parameters output structure.
                parameters.([variable '_fig']) = fig;

            end
        end
    end
end 
