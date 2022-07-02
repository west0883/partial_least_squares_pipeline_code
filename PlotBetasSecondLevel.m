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

    % Pull out betas to use going forward (using just the intercepts).
    betas = parameters.results.BETA(1, :);
   
    % Adjust Betas based on z-score sigma. % First row is constant estimate
    % If user says so
    if isfield(parameters, 'adjust_beta') && parameters.adjust_beta
        betas_adjusted = betas ./ parameters.dataset_info.zscoring.brainData.sigma' .*  parameters.dataset_info.zscoring.responseVariables.sigma; 
    else
        betas_adjusted = betas;
    end

    % If user wants to use significance for plotting,
    if isfield(parameters, 'useSignificance') && parameters.useSignificance
        
        % Only keep/plot beta values that reach significance 
        betas_adjusted = betas_adjusted .* parameters.significance'; 

    end
    
    % Get comparison iterator for subplot location. 
    iterator_location = strcmp(parameters.keywords, {'comparison_iterator'});
    comparison_iterator = parameters.values{iterator_location};

    % Get comparison name for subplot title. 
    name_location = strcmp(parameters.keywords, {'comparison'});
    comparison = parameters.values{name_location};

    % Make a colormap with cbrewer; 
    cmap= flipud(cbrewer('div', 'RdBu', 512, 'nearest'));

    % Start plotting. 
    
    % If categorical, 
    if strcmp(parameters.comparison_type, 'categorical')

        % If there isn't a figure for this yet, make one.
        if ~isfield(parameters, 'fig')
            fig = figure;
            fig.WindowState = 'maximized';

            % Make title.
            title_string = ['Betas, second level ' parameters.comparison_type];
            title_string = strrep(title_string, '_', ' ');
            sgtitle(title_string);

            % Put into output structure.
            parameters.fig = fig;
        end

        % Just need one plot per comparison.
        holder = NaN(parameters.number_of_sources, parameters.number_of_sources);
        holder(parameters.indices) = betas_adjusted;
    
        % Make a fitted color range for this plot.
        extreme = max(max(holder, [], 'all', 'omitnan'), abs(min(holder, [], 'all', 'omitnan')));
        color_range = [-extreme extreme]; 

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
                title_string = ['Betas, ' variable ' second level' parameters.comparison_type];
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
            try
            holder(parameters.indices) = betas_adjusted((variablei - 1) * numel(parameters.indices) + [1:numel(parameters.indices)]);
            catch
            end
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
        
            % Make a fitted color range for this plot.
            extreme = max(max(betas_separated_variables(:,:, variablei), [], 'all', 'omitnan'), abs(min(betas_separated_variables(:,:, variablei), [], 'all', 'omitnan')));
            color_range = [-extreme extreme]; 
            
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

end 
