classdef NamedMatrix < double
 
% FIXME: binaryOp must NOT check the size for mtimes() etc.
% FIXME: 'format bank' is not yet implemented
% FIXME: What to do with matrix title after vertcat/horzcat?
% FIXME: binaryOp() obj argument
% FIXME: display empty objects
% FIXME: reshape(), permute() etc.
% FIXME: default import functions from XML/JSON/CSV/XLS files
% FIXME: display of POWERS in 'short' etc. formats

% FIXME: fix the setters/getters

% FIXME: 
%
% Example: 
% clear classes
% W = NamedMatrix(255*randn(2), [], ...
%    {'Jenkins case'; 'Crude estimation of Redmine case'},...
%    {'Mean' 'Perceived risk'})
% 
% [W(:,1) W(:,2)] copies the wrong column titles
% [W(:,1) rand(2,1) W(:,2)] does not work
% Neither does [W(:,1) rand(2,1)]
% Neither does W(:,1) = W(:,2)
% Indexing does not work correctly (W(:,1) looks OK, but W(:,2) copies the
%     wrong column name
%
% ...Subsasgn is also strange

    
    properties (Hidden, Access = private)
        
        matrixTitle
        columnTitles
        rowTitles   
        
        originalClass  
        
        padding = 3
        
    end
    
    properties (Hidden, Dependent, Access = private)
        data               % the data, cast back to its original class
        nominalFieldWidth  % field widths of printed versions of all classes for 
                           % all format settings
    end
    
    methods
        
        %% Constructor
        function obj = NamedMatrix(M, matrixTitle, rowTitles, columnTitles)
            obj = obj@double(M);
            
            % Copy constructor
            if nargin == 1 && isa(M, 'NamedMatrix')
                obj.matrixTitle = M.matrixTitle;
                obj.columnTitles = M.columnTitles;
                obj.rowTitles    = M.rowTitles;                
                obj.originalClass   = M.originalClass;
                return
            end
            
            % Normal contructor
            obj.originalClass = class(M);
            
            obj.matrixTitle  = matrixTitle;
            obj.rowTitles    = rowTitles;
            obj.columnTitles = columnTitles;
            
            if ~isempty(obj.matrixTitle) && ~ischar(obj.matrixTitle)
                error('NamedMatrix:invalid_title',...
                    'Matrix title must be a string.');
            end
            
            if ischar(obj.rowTitles)
                obj.rowTitles = {obj.rowTitles}; end
            if ischar(obj.columnTitles)
                obj.columnTitles = {obj.columnTitles}; end
               
            if ~iscellstr(obj.rowTitles) || ~iscellstr(obj.columnTitles)
                error('NamedMatrix:invalid_column_titles',...
                    ['Invalid title format.\n',...
                    'Row and column titles must be strings or cell arrays of strings.']);
            end
            
            obj.rowTitles = [
                obj.rowTitles(:); 
                repmat({''}, size(obj,1)-numel(obj.rowTitles),1)
            ]; 
            obj.columnTitles = [
                obj.columnTitles(:); 
                repmat({''}, size(obj,1)-numel(obj.columnTitles),1)
            ];          
                   
        end

        
        %% Getters/setters for titles
        
        function T = matrixHeader(obj, newTitle)
            T = obj.matrixTitle; 
            % TODO: put error checks from constructor here, and call this
            % thing in the constructor
            if nargin > 1
                obj.matrixTitle = newTitle; end            
        end
        
        function T = rowHeaders(obj, newTitles)
            T = obj.rowTitles; 
            % TODO: put error checks from constructor here, and call this
            % thing in the constructor
            if nargin > 1
                obj.rowTitles = newTitles; end            
        end
        
        function T = columnHeaders(obj, newTitles)
            T = obj.columnTitles; 
            % TODO: put error checks from constructor here, and call this
            % thing in the constructor
            if nargin > 1
                obj.columnTitles = newTitles; end            
        end
        
        
        %% Implement the basics of any numeric class
        
        function newObj = subsref(obj,S)  
            try                
                switch S.type
                    case '.'
                        error('NamedMatrix:nonStrucReference',...
                            'Attempt to reference field of non-structure array.');
                    case '{}'
                        cellRefFromNonCell
                        error('NamedMatrix:cellRefFromNonCell',...
                            'Cell contents reference from a non-cell array object.');
                                                
                    case '()'
                        % Do this first; any errors will pop up quickly that way                        
                        newData = subsref(obj.data, S);
                        
                        % This one always gets copied
                        newMatrixTitle  = obj.matrixTitle;
                        
                        % Empties are treated differently; 
                        if isempty(obj)                             
                            newRowTitles    = obj.rowTitles;
                            newColumnTitles = obj.columnTitles;
                            
                        else
                            
                            % Cut off any trailing ones
                            S.subs = S.subs(1 : min(...
                                end, max(...
                                    ndims(obj),find(~cellfun(@(x)isequal(x,1),S.subs(:)),1,'last')...
                                )...
                            ));
                            
                            
                            
                            newRowTitles    = obj.rowTitles(1:size(newData,1));
                            newColumnTitles = obj.columnTitles(1:size(newData,2));
                            
                        end
                        
                        % We can now create the new object
                        newObj = NamedMatrix(...
                            newData, newMatrixTitle, newRowTitles, newColumnTitles);                                                
                end
                
            catch ME
                throwAsCaller(ME);
            end
        end
        
        function newObj = subsasgn(obj,varargin)
            try                
                newData = subsasgn(obj.data, varargin{:});                
                newObj = NamedMatrix(newData, ...
                    obj.matrixTitle, obj.rowTitles(1:size(newData,1)), obj.columnTitles(1:size(newData,2)));
                
            catch ME
                throwAsCaller(ME);
            end
        end
        
        function newObj = cat(dim, varargin)
            try
                switch dim
                    case {0 1} % NOTE: same behavior as in built-in CAT()
                        newObj = vertcat(varargin{:});
                    case 2
                        newObj = horzcat(varargin{:});
                        
                    otherwise
                        if ~isscalar(dim) || ~isfinite(dim) || imag(dim)~=0 || dim < 0
                            error('NamedMatrix:catenate:invalidDimension',...
                                'Dimension must be a finite integer.');
                        end
                        
                        % TODO: the rest. Adjust the display() function as well
                        
                end
                
            catch ME                
                throwAsCaller(ME);
            end
        end
        
        function newObj = horzcat(varargin)
            
            % Obvious error
            sz1 = cellfun(@(x)size(x,1), varargin, 'UniformOutput', false);            
            if ~isequal(sz1{:})
                % (Yes, even if some are empty)
                throwAsCaller(MException('NamedMatrix:horzcat:dimensionMismatch',...
                    'CAT arguments dimensions are not consistent.'));                
            end
            
            % An abbreviation, because we need it a lot but it's kind of
            % long and distracting
            uf = {'UniformOutput', false};
            
            % Lookup all the Named Matrices (concatenation with other types
            % is allowed)
            V = varargin;
            I = cellfun('isclass', V, 'NamedMatrix');
            
            % Can't concatenate if any of the row titles are different
            rT = cellfun(@(x)x.rowTitles(1:size(x,1)), V(I), uf{:});            
            if ~isequal(rT{:})
                throwAsCaller(MException('NamedMatrix:horzcat:dimensionMismatch',...
                    'HORZCAT only defined for NamedMatrices with equal row titles.'));
            end
                                    
            % The new row titles are taken from the NamedMatrix with 
            % the longest list
            rT = cellfun(@(x)x.rowTitles, V(I), uf{:});
            [~,maxInd] = max(cellfun('prodofsize', rT));
            newRowTitles = V{maxInd}.rowTitles;
            clear rT
            
            % If any of the matrix titles differ, just give warning
            mT = cellfun(@(x)x.matrixTitle, V(I), uf{:});
            if ~isequal(mT{:})
                warning('NamedMatrix:horzcat:matrixTitlesDiffer',...
                    'Concatenating with different matrix titles.');
            end
            clear mT
            
            % Take the matrix title of this object. 
            newMatrixTitle = varargin{1}.matrixTitle;
            
            
            % The data for the new NamedMatrix is the horzcat() of all the
            % data. In other words, delegate all the work and further error
            % checking to the built-in horzcat():
            V(I) = cellfun(@(x)x.data, V(I), uf{:});
            newData = horzcat(V{:});
              
            % Concatenate the column titles. Pad with empties when 
            % concatenating with matrices which are not named.
            sz2 = cellfun(@(x)size(x,2), V, uf{:});
            newColumnTitles    = cell(nargin,1);
            newColumnTitles(I) = cellfun(@(x,y)x.columnTitles(1:y), ...
                varargin(I), sz2, uf{:});
            E = cellfun('isempty', newColumnTitles);
            newColumnTitles(E) = cellfun(@(x)repmat({''}, x,1), sz2(E), uf{:});
            newColumnTitles = cat(1, newColumnTitles{:});
                      
            % The new NamedMatrix can now be created
            newObj  = NamedMatrix(newData, newMatrixTitle, newRowTitles, newColumnTitles);
        end
        
        function newObj = vertcat(varargin)
            
            % Obvious error
            sz2 = cellfun(@(x)size(x,2), varargin, 'UniformOutput', false);            
            if ~isequal(sz2{:})
                % (Yes, even if some are empty)
                error('NamedMatrix:horzcat:dimensionMismatch',...
                    'CAT arguments dimensions are not consistent.');
            end
            
            % An abbreviation, because we need it a lot but it's kind of
            % long and distracting
            uf = {'UniformOutput', false};
            
            % Lookup all the Named Matrices (concatenation with other types
            % is allowed)
            V = varargin;
            I = cellfun('isclass', V, 'NamedMatrix');
                       
            
            % Can't concatenate if any of the column titles are different            
            cT = cellfun(@(x)x.rowTitles(1:size(x,1)), V(I), uf{:});            
            if ~isequal(cT{:})
                throwAsCaller(MException('NamedMatrix:vertcat:dimensionMismatch',...
                    'VERTCAT only defined for NamedMatrices with equal column titles.'));
            end
                                    
            % The new column titles are taken from the NamedMatrix with 
            % the longest list
            cT = cellfun(@(x)x.columnTitles, V(I), uf{:});
            [~,maxInd] = max(cellfun('prodofsize', cT));
            newColumnTitles = V{maxInd}.columnTitles;
            clear cT
            
            % If any of the matrix titles differ, just give warning
            mT = cellfun(@(x)x.matrixTitle, V(I), uf{:});
            if ~isequal(mT{:})
                warning('NamedMatrix:vertcat:matrixTitlesDiffer',...
                    'Concatenating with different matrix titles.');
            end
            clear mT
            
            % Take the matrix title of this object. 
            newMatrixTitle = varargin{1}.matrixTitle;
                        
            
            % The data for the new NamedMatrix is the vertcat() of all the
            % data. In other words, delegate all the work and error
            % checking to the built-in vertcat():
            V(I) = cellfun(@(x)x.data, V(I), uf{:});
            newData = vertcat(V{:});
              
            % Concatenate the row titles. Pad with empties when 
            % concatenating with matrices which are not named.
            sz1 = cellfun(@(x)size(x,1), V, uf{:});
            newRowTitles    = cell(nargin,1);
            newRowTitles(I) = cellfun(@(x,y)x.rowTitles(1:y), ...
                varargin(I), sz1, uf{:});
            E = cellfun('isempty', newRowTitles);
            newRowTitles(E) = cellfun(@(x)repmat({''}, x,1), sz1(E), uf{:});
            newRowTitles = cat(1, newRowTitles{:});
                      
            % The new NamedMatrix can now be created
            newObj  = NamedMatrix(newData, newMatrixTitle, newRowTitles, newColumnTitles);
            
        end
        
        
        %% Type casting
        
        function C = class(obj)
            C = obj.originalClass; end
        
        function newObj = cast(obj, newType)
            try
                newObj = obj;
                if ~strcmp(obj.originalClass, newType)
                    newObj = NamedMatrix(builtin(newType, obj.data), ...
                        obj.matrixTitle, obj.rowTitles, obj.columnTitles);
                end
            catch ME
                ME = MException('NamedMatrix:invalid_cast',...
                    'Invalid cast to ''%s'' or class not found.', newType);
                throwAsCaller(ME);
            end
        end
        
        function newObj = single(obj),  newObj = obj.cast('single');  end        
        function newObj = logical(obj), newObj = obj.cast('logical'); end        
        function newObj = char(obj),    newObj = obj.cast('char');    end        
        function newObj = int8(obj),    newObj = obj.cast('int8');    end        
        function newObj = uint8(obj),   newObj = obj.cast('uint8');   end        
        function newObj = int16(obj),   newObj = obj.cast('int16');   end        
        function newObj = uint16(obj),  newObj = obj.cast('uint16');  end        
        function newObj = int32(obj),   newObj = obj.cast('int32');   end        
        function newObj = uint32(obj),  newObj = obj.cast('uint32');  end        
        function newObj = int64(obj),   newObj = obj.cast('int64');   end        
        function newObj = uint64(obj),  newObj = obj.cast('uint64');  end
        
        
        %% Math operators
        
        function newObj = ctranspose(obj)
            % NOTE: also col/row titles are swapped
            newObj = NamedMatrix(ctranspose(obj.data), ...
                obj.matrixTitle, obj.columnTitles, obj.rowTitles);
        end
        
        function newObj = transpose(obj)
            % NOTE: also col/row titles are swapped
            newObj = NamedMatrix(transpose(obj.data), ...
                obj.matrixTitle, obj.columnTitles, obj.rowTitles);
        end
        
        
        function newObj = uplus(obj)
            newObj = NamedMatrix(uplus(obj.data), ...
                obj.matrixTitle, obj.rowTitles, obj.columnTitles);
        end
        
        function newObj = uminus(obj)
            newObj = NamedMatrix(uminus(obj.data), ...
                obj.matrixTitle, obj.rowTitles, obj.columnTitles); 
        end
        
        
        function newObj = plus(obj, other)
            try
                if isa(obj, 'NamedMatrix')
                    newObj = obj.binaryOp(obj,other,'plus');
                else
                    newObj = other.binaryOp(obj,other,'plus');
                end
            catch ME
                throwAsCaller(ME);
            end
        end
        
        function newObj = minus(obj, other)
            try
                if isa(obj, 'NamedMatrix')
                    newObj = obj.binaryOp(obj,other,'minus');
                else
                    newObj = other.binaryOp(obj,other,'minus');
                end
            catch ME
                throwAsCaller(ME);
            end
        end
        
        function newObj = rdivide(obj, other)            
            try
                if isa(obj, 'NamedMatrix')
                    newObj = obj.binaryOp(obj,other,'rdivide');
                else
                    newObj = other.binaryOp(obj,other,'rdivide');
                end
            catch ME
                throwAsCaller(ME);
            end            
        end
        
        function newObj = ldivide(obj, other)            
            try
                if isa(obj, 'NamedMatrix')
                    newObj = obj.binaryOp(obj,other,'ldivide');
                else
                    newObj = other.binaryOp(obj,other,'ldivide');
                end
            catch ME
                throwAsCaller(ME);
            end
        end
        
        function newObj = idivide(obj, other)            
            try
                if isa(obj, 'NamedMatrix')
                    newObj = obj.binaryOp(obj,other,'idivide');
                else
                    newObj = other.binaryOp(obj,other,'idivide');
                end
            catch ME
                throwAsCaller(ME);
            end
        end
        
        function newObj = times(obj, other)            
            try
                if isa(obj, 'NamedMatrix')
                    newObj = obj.binaryOp(obj,other,'times');
                else
                    newObj = other.binaryOp(obj,other,'times');
                end
            catch ME
                throwAsCaller(ME);
            end
        end
        
        function newObj = power(obj, other)            
            try
                if isa(obj, 'NamedMatrix')
                    newObj = obj.binaryOp(obj,other,'power');
                else
                    newObj = other.binaryOp(obj,other,'power');
                end
            catch ME
                throwAsCaller(ME);
            end
        end
        
        
        % These all change the dimensions: 
        
        % TODO: mpower
        % TODO: mtimes
        % TODO: mldivide
        % TODO: mrdivide
        
        
        %% The display function
        
        function display(obj)
            
            % 'loose' adds extra newlines, 'compact' does not
            compact = strcmp(get(0,'FormatSpacing'), 'compact');
            if compact
                fprintf(1, [inputname(1) ' =\n']);
            else
                fprintf(1, ['\n' inputname(1) ' =\n\n']);
            end
                        
            N = obj.nominalFieldWidth;
            % Complex data requires twice as much space, except for 
            % FORMAT + (ignores imaginary components)
            if any(imag(obj.data(:))) && ~strcmp(get(0,'format'), '+')
                N = 2*N-1; end
            C = cellfun('prodofsize', obj.columnTitles(1:size(obj,2))) + obj.padding;
            R = cellfun('prodofsize', obj.rowTitles   (1:size(obj,1))) + obj.padding;                                    
            P = max([N; C(:)']);
                                    
            colDispStr  = regexprep(num2str(P),'\s*([0-9]+)', ['%$1' 's']);
            rowDispStr  = ['%' num2str(max(R)) 's'];
                       
            if ~isempty(obj.matrixTitle)
                titleStart = ceil((sum(P)+max(R))/2 - numel(obj.matrixTitle)/2);
                fprintf(1, repmat(' ', 1,titleStart-1));
                fprintf(1, [obj.matrixTitle '\n']);
                if ~compact
                    fprintf(1, '\n'); end
            end
            
            % Print column titles
            fprintf(1, rowDispStr, '');
            fprintf(1, colDispStr, obj.columnTitles{:});
            fprintf(1, '\n');
            
            % Print row titles and data. Field width is proportional to
            % data and/or title
            cObj = obj.data;                
            for ii = 1:size(obj,1)
                fprintf(1, rowDispStr, obj.rowTitles{ii});
                for jj = 1:size(obj,2)
                    fprintf(1,repmat(' ', 1,P(jj)-N(ii,jj)+1));
                    disp(cObj(ii,jj));
                    % DISP of char() has other behavior than DISP of numeric class                    
                    if compact || strcmp(obj.originalClass, 'char')
                        fprintf(1,'\b');
                    else
                        fprintf(1,'\b\b');                        
                    end
                end
                fprintf(1,'\n');
            end
            
            if ~compact
                fprintf(1,'\n'); end
            
        end
        
        
        %% Getters for dependent props
                
        function C = get.data(obj)            
            try
                C = eval(['builtin(''' obj.originalClass ''', double(obj));']);             
            catch ME
                ME2 = MException('NamedMatrix:cast_error',...
                    'Could not cast data back to their original class.');
                throwAsCaller(addCause(ME2,ME));
            end
        end
        
        function F = get.nominalFieldWidth(obj)
            
            %   double  single  logical  char  (u)int8  (u)int16 (u)int32 (u)int64
            fieldWidths = [...
                11      11      10       2     6        8        13       22       % short
                21      13      10       2     6        8        13       22       % long
                14      14      10       2     6        8        13       22       % short e
                27      18      10       2     6        8        13       22       % long e
                14      15      10       2     6        8        13       22       % short g
                27      18      10       2     6        8        13       22       % long g
                17      17      10       2     6        8        13       22       % short eng
                26      17      10       2     6        8        13       22       % long eng
                20      12       5       2     6        8        13       22       % hex
                2       2        2       2     2        2         2        2       % +
                ];
            
            %TODO: (bank) has variable field width; compute it based on data
            
            % Field widths are pretty specific: 
            % - Double or single matrices which have exact integer entries 
            %   are displayed as uint8 or uint32, with some extra spacing 
            %   (or not)
            % - In 'format short' and 'format long', if the magnitude of the 
            %   value exceeds a certain threshold, an overall exponent will 
            %   be displayed that is to be multiplied with all the values. 
            %   The values are printed differently than before. It's nicest 
            %   to have NAMEDMATRIX display the exponents on each *column*,
            %   which complicates things a bit
            
            
            F     = zeros(size(obj));            
            cObj  = obj.data;
            nonE  = any(strcmp(get(0,'format'), {'short', 'long'}));
            
            % Field width must be derived for all individual components
            for ii = 1:size(obj,1)
                for jj = 1:size(obj,2)
                    
                    dispClass = obj.originalClass;
                    offSet = 0;
                    
                    % Entry is exact integer
                    if ~any(strcmp(dispClass, ...
                            {'char' 'int8' 'uint8' 'int16' 'uint16', 'int32' 'uint32' 'uint64' 'uint64'}))                        
                        rcObj = round(cObj(ii,jj));
                        if rcObj==cObj(ii,jj)                            
                            if rcObj <= 1e3-1
                                dispClass = 'uint8';
                                offSet = 1;
                            elseif rcObj <= 1e9-1
                                dispClass = 'uint32';
                            end
                        end
                    end
                    
                    % We have 'format long' or 'format short' and the entry
                    % will be displayed with an exponent
                    if nonE
                        % TODO
                    end
                    
                    % Do the lookup
                    col = ~cellfun('isempty', regexp(dispClass, ...
                        {'double' 'single' 'logical' 'char' 'int8' 'int16' 'int32' 'int64'}));
                    row = strcmp(get(0,'format'),...
                        {'short', 'long', 'shortE', 'longE', 'shortG', 'longG', 'shortEng', 'longEng', 'hex', '+', 'bank'});
                    
                    F(ii,jj) = fieldWidths(row, col) + offSet;
                                        
                end
            end             
        end
        
    end
    
    methods (Access = private)
        
                
        function newObj = binaryOp(obj, one, two, fcn) %#ok<MANU>
            try
                if isa(one, 'NamedMatrix') && isa(two, 'NamedMatrix')
                    if ~isequal(size(one), size(two))
                        error('NamedMatrix:dimagree',...
                            'Matrix dimensions must agree.');
                    end
                    if isequal(one.columnTitles, two.columnTitles) &&...
                            isequal(one.rowTitles, two.rowTitles)
                        newObj = NamedMatrix(builtin(fcn, one.data, two.data), ...
                            one.matrixTitle, one.rowTitles, one.columnTitles);
                    else
                        error('NamedMatrix:title_mismatch',...
                            'Undefined function or method ''%s'' for NamedMatrices with dissimilar row/column titles.', fcn);
                    end
                else
                    if isa(one, 'NamedMatrix');
                        newObj = NamedMatrix(builtin(fcn, one.data, two), ...
                            one.matrixTitle, one.rowTitles, one.columnTitles);
                    else
                        newObj = NamedMatrix(builtin(fcn, one, two.data), ...
                            two.matrixTitle, two.rowTitles, two.columnTitles);
                    end
                end
            catch ME
                throwAsCaller(ME);
            end
        end        
    end
    
end


