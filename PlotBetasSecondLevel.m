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

    % Get figure type for this comparison.
    figure_type = parameters.this_comparison_set(comparison_iterator).figure_type;

    % Check if this comparison needs a special color range.
    color_indices = strcmp(parameters.color_range.specials(:,1), comparison);
    if any(color_indices)
        color_range_special = parameters.color_range.specials(color_indices, 2:3);
    else
        color_range_special = {};
    end

    % Check if this needs to be flipped.
    if isfield(parameters.this_comparison_set(comparison_iterator), 'plotMultiplier')
        
        % Multiply by plot multipier.
        betas_adjusted = betas_adjusted .* parameters.this_comparison_set(comparison_iterator).plotMultiplier;

    end

    % Make a colormap with cbrewer; 
    cmap= flipud(cbrewer('div', 'RdBu', 2000, 'linear'));

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
            
            % If adjusted, use a standard color range for each plot.
            if isfield(parameters, 'adjustBetas') && parameters.adjustBetas
                color_range = parameters.color_range.(figure_type).categorical;
            % If not adjusted, make a fitted color range for this plot.
            else
                extreme = max(max(holder, [], 'all', 'omitnan'), abs(min(holder, [], 'all', 'omitnan')));
                color_range = [-extreme extreme]; 
            end

            % If the color range for this comparison was special,
            if isfield(parameters, 'adjustBetas') && parameters.adjustBetas && ~isempty(color_range_special)
                color_range = color_range_special{2};
            end
    
            % Plot.
            subplot(subplot_rows, subplot_columns, comparison_iterator); imagesc(holder); 
             % Don't use the color range if both values are 0.
            if any(color_range)
                caxis(color_range);
            end
            colormap(cmap); colorbar; 
           
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
                    color_range = parameters.color_range.(figure_type).(variable);
                % If not adjusted, make a fitted color range for this plot.
                else
                    extreme = max(max(betas_separated_variables(:,:, variablei), [], 'all', 'omitnan'), abs(min(betas_separated_variables(:,:, variablei), [], 'all', 'omitnan')));
                    color_range = [-extreme extreme]; 
                end
                
                % If the color range for this comparison was special,
                if isfield(parameters, 'adjustBetas') && parameters.adjustBetas && ~isempty(color_range_special)
                    
                    % See if the "special" applies to this variable
                    if strcmp(color_range_special{1}, variable)

                        % If it does, change color range to that.
                        color_range = color_range_special{2};
                    end
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

        % Make grid lines at specific places. Minor & major grid lines.
        grid_locations_major = 0.5:5:parameters.number_of_sources + 0.5;
        grid_locations_minor = setdiff(0.5:1:parameters.number_of_sources + 0.5, grid_locations_major);

        % If categorical, 
        if strcmp(parameters.comparison_type, 'categorical')

            % Make holder matrix
            holder = NaN(parameters.number_of_sources, parameters.number_of_sources);
            holder(parameters.indices) = betas_adjusted;
            
            % Duplicate betas across diagonal.
            indices_upper = find(triu(ones(parameters.number_of_sources), 1));
            betas_flipped = holder';
            elements_upper = betas_flipped(indices_upper);
            holder(indices_upper) = elements_upper;

            % Make diagonal blank = 0
            for i = 1:parameters.number_of_sources
                holder(i, i) = 0;
            end

            % Make a figure
            fig = figure;

            % If adjusted, use a standard color range for each plot.
            if isfield(parameters, 'adjustBetas') && parameters.adjustBetas && isfield(parameters, 'useColorRange') && parameters.useColorRange
                color_range = parameters.color_range.(figure_type).categorical;
            % If not adjusted, make a fitted color range for this plot.
            else
                extreme = max(max(holder, [], 'all', 'omitnan'), abs(min(holder, [], 'all', 'omitnan')));
                if extreme == 0
                    color_range = parameters.color_range;
                else
                    color_range = [-extreme, extreme]; 
                end
               
            end

            % If the color range for this comparison was special,
            if ~isempty(color_range_special)
                % If it was, change color range to that.
                color_range = color_range_special{2};
            end

            % Plot 
            imagesc(holder); axis square;
            Colorbar_handle = colorbar; caxis(color_range); colormap(cmap);
            
            % Get axis handle.
            ax = gca;

            title_string = comparison; 
            ax.TitleFontSizeMultiplier = 0.5;
            title_handle = title(strrep(title_string, '_', ' '));
            set(title_handle,'position',get(title_handle,'position') - [0 2 0]);
            
            % Put in grid lines.

            % Minor grid lines. 
            ax.XAxis.MinorTick = 'on';
            ax.YAxis.MinorTick = 'on';
            ax.XAxis.MinorTickValues = grid_locations_minor;
            ax.YAxis.MinorTickValues = grid_locations_minor;
            ax.MinorGridLineStyle = '-'; % Make solid lines.
            ax.MinorGridColor = [0.75 0.75 0.75];
            ax.MinorGridAlpha = 1;
            ax.XMinorGrid = 'on';
            ax.YMinorGrid = 'on';
            ax.Layer = 'top';

            % Major grid lines. (adjust style/width so you can see them)
            grid on;
            ax.XTick = grid_locations_major;
            ax.YTick = grid_locations_major; 
            ax.GridColor = [0, 0, 0]; % Make darker than minor grid.
            ax.GridAlpha = 1; % Make more opaque.
            ax.Layer = 'top';
          
    
            % Make ticks themselves invisible.
            set(gca, 'TickLength',[0 0]);
    
            % Redo tick labels 
            ax.XTickLabel = {'', '5', '10', '15', '20', '25', '30'};
            ax.YTickLabel = {'', '5', '10', '15', '20', '25', '30'};
            set(ax, 'yticklabel');
    
            % Remove background color.
            ax.Color = 'none';
    
            % Make outline of box thicker. (Controls width of all grid
            % lines, too??)
            ax.LineWidth = 0.4;

            % Make figure background white.
            fig.Color = 'w';

            % Make colorbar outline thicker
            Colorbar_handle.LineWidth = 0.4;

            % Make tick labels larger. (Don't make bold because they
            % weren't bold in the spontaneous paper).
            ax.FontSize = 18;

            % Add x and y axis labels. 
            ax.XLabel.String = 'node';
            ax.YLabel.String = 'node';
            ax.XLabel.FontSize = 24;
            ax.YLabel.FontSize = 24;

            % Scoot y label slightly to left.
            positions = ax.YLabel.Position;
            ax.YLabel.Position = [positions(1) - 0.3, positions(2), positions(3)];

            % For colorbar, keep only labels for 0 and extremes.
            Colorbar_handle.Ticks = [color_range(1), 0, color_range(2)];

            % Make colorbar ticks certain length
            Colorbar_handle.TickLength = .015;

            % Make colorbar tick labels certain size.
            Colorbar_handle.FontSize = 18;

            % Make colorbar label. (Don't make bold because they
            % weren't bold in the spontaneous paper).
            Colorbar_handle.Label.String = { '\Delta{\it r}'}; % Change in r (delta symbol, italisized r)
            
                                    %{'sig. change in';'correlation coeff'};
            Colorbar_handle.Label.FontSize = 24;
            Colorbar_handle.Label.Rotation = -90;

            % Move colorbar label to the right.
            positions = Colorbar_handle.Label.Position;
            Colorbar_handle.Label.Position = [positions(1) + 1, positions(2), positions(3)];

            % Make diagonals black. 
            hold on; 
            a3 = ones(32,32); for i = 1:32; a3(i,i) = 0; end % Make a matrix showing where the diagnal is
            a3 = repmat(a3, 1,1,3);
            a = zeros(32,32); for i = 1:32; a(i,i) = 1; end % Make a matrix of alpha values
            im = image(a3, 'CDataMapping','direct'); % Make an image (use image NOT imagsc).
            alpha(im, a); % Aplly alpha values. 

            % Put figure into parameters output structure.
            parameters.fig = fig;
    
        % If continuous, 
        else

            % Use the comparisons matrix to get the variables you're using for
            % this comparison.
            variablesToUse = parameters.this_comparison_set(comparison_iterator).variablesToUse;

            % For saving, 
            parameters.dont_save = repmat({true}, numel(parameters.continuous_variable_names), 1);

            % Separate out each beta matrix per variable
            for variablei = 1:numel(variablesToUse)

                % Get the variable name
                variable = erase(variablesToUse{variablei}, '_vector');

                % Get units for colorbar label based on variable.
                index = find(strcmp(parameters.continuous_variable_names, variable));

                switch index

                    case 1 % speed
                        units = 'per cm/s';

                    case 2 % accel
                        units = 'per cm/s/s';

                    case 3 % duration
                        units = 'per s';

                    case 4 % pupil diameter
                        units = 'per % max diameter';
                end

                % For saving, see if this variable is in the list of continuous
                % variables. 
                parameters.dont_save{strcmp(parameters.continuous_variable_names, variable)} = false;
    
                % Make a 2D holder (because lists of indices are weird).
                betas_separated = NaN(parameters.number_of_sources, parameters.number_of_sources);
                
                % Separate
                betas_separated(parameters.indices) = betas_adjusted((variablei - 1) * numel(parameters.indices) + [1:numel(parameters.indices)]);

                % Rename back to holder to keep things easier
                holder = betas_separated;
 
                % Duplicate betas across diagonal
                indices_upper = find(triu(ones(parameters.number_of_sources), 1));
                betas_flipped = holder';
                elements_upper = betas_flipped(indices_upper);
                holder(indices_upper) = elements_upper;

                % Make diagonal blank = 0
                for i = 1:parameters.number_of_sources
                    holder(i, i) = 0;
                end

                % Make a figure for this variable.
                fig = figure;
                
                % If adjusted, use a standard color range for each plot.
                if isfield(parameters, 'adjustBetas') && parameters.adjustBetas && isfield(parameters, 'useColorRange') && parameters.useColorRange
                    color_range = parameters.color_range.(figure_type).(variable);
                % If not adjusted, make a fitted color range for this plot.
                else
                    extreme = max(max(holder, [], 'all', 'omitnan'), abs(min(holder, [], 'all', 'omitnan')));
                    
                    if extreme == 0
                        color_range = parameters.color_range;
                    else
                        color_range = [-extreme, extreme]; 
                    end
                end

                % See if color range for this comparison was special.
                if ~isempty(color_range_special)
                    % If it was, see if the "special" applies to this variable
                    if strcmp(color_range_special{1}, variable)
    
                        % If it does, change color range to that.
                        color_range = color_range_special{2};
                    end
                end

                % Plot 
                imagesc(holder); axis square;
                Colorbar_handle = colorbar; caxis(color_range); colormap(cmap);

                % Get axis handle
                ax = gca;

                % Make a title
                title_string = [variable ', ' comparison]; 
                ax.TitleFontSizeMultiplier = 0.5;
                title_handle = title(strrep(title_string, '_', ' '));
                set(title_handle,'position',get(title_handle,'position') - [0 2 0]);
    
                % Put in grid lines.
                % Minor grid lines. 
                ax.XAxis.MinorTick = 'on';
                ax.YAxis.MinorTick = 'on';
                ax.XAxis.MinorTickValues = grid_locations_minor;
                ax.YAxis.MinorTickValues = grid_locations_minor;
                ax.MinorGridLineStyle = '-'; % Make solid lines.
                ax.MinorGridColor = [0.75 0.75 0.75];
                ax.MinorGridAlpha = 1;
                ax.XMinorGrid = 'on';
                ax.YMinorGrid = 'on';
                ax.Layer = 'top';
    
                % Major grid lines. (adjust style/width so you can see them)
                grid on;
                ax.XTick = grid_locations_major;
                ax.YTick = grid_locations_major; 
                ax.GridColor = [0, 0, 0]; % Make darker than minor grid.
                ax.GridAlpha = 1; % Make more opaque.
                ax.Layer = 'top';
    
                % Make ticks themselves invisible.
                set(gca, 'TickLength',[0 0]);
        
                % Redo tick labels 
                ax.XTickLabel = {'', '5', '10', '15', '20', '25', '30'};
                ax.YTickLabel = {'', '5', '10', '15', '20', '25', '30'};
                set(ax, 'yticklabel');
        
                % Remove background color.
                ax.Color = 'none';
        
                % Make outline of box thicker. (Controls width of all grid
                % lines, too??)
                ax.LineWidth = 0.4;

                 ax.Layer = 'top';

               % box_handle = get(boxFrame);
    
                % Make figure background white.
                fig.Color = 'w';
    
                % Make colorbar outline thicker
                Colorbar_handle.LineWidth = 0.4;
    
                % Make tick labels larger. (Don't make bold because they
                % weren't bold in the spontaneous paper).
                ax.FontSize = 18;
    
                % Add x and y axis labels. 
                ax.XLabel.String = 'node';
                ax.YLabel.String = 'node';
                ax.XLabel.FontSize = 24;
                ax.YLabel.FontSize = 24;
    
                % Scoot y label slightly to left.
                positions = ax.YLabel.Position;
                ax.YLabel.Position = [positions(1) - 0.3, positions(2), positions(3)];
    
                % For colorbar, keep only labels for 0 and extremes.
                Colorbar_handle.Ticks = [color_range(1), 0, color_range(2)];
    
                % Make colorbar ticks certain length
                Colorbar_handle.TickLength = .015;
    
                % Make colorbar tick labels certain size.
                Colorbar_handle.FontSize = 18;
    
                % Make colorbar label. (Don't make bold because they
                % weren't bold in the spontaneous paper). Include units for
                % continuous.
                Colorbar_handle.Label.String =   {['\Delta{\it r} ' units]};
                    %{'sig. change in'; 'correlation coeff'};
                Colorbar_handle.Label.FontSize = 24;
                Colorbar_handle.Label.Rotation = -90;
    
                % Move colorbar label to the right.
                positions = Colorbar_handle.Label.Position;
                Colorbar_handle.Label.Position = [positions(1) + 3, positions(2), positions(3)];

                % Make diagonals black. 
                hold on; 
                a3 = ones(32,32); for i = 1:32; a3(i,i) = 0; end % Make a matrix showing where the diagnal is
                a3 = repmat(a3, 1,1,3);
                a = zeros(32,32); for i = 1:32; a(i,i) = 1; end % Make a matrix of alpha values
                im = image(a3, 'CDataMapping','direct'); % Make an image (use image NOT imagsc).
                alpha(im, a); % Aplly alpha values. 
 
                % Rename figure handle, put into parameters output structure.
                parameters.([variable '_fig']) = fig;

            end
        end
    end
end 
