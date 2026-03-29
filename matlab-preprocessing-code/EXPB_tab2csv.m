% match parent invasive with surface growth dataset
% 
% Kai Li
% 25 Mar 2026

clear
clc

load("invasive_B4.mat");
load("surface_B4.mat");

%% EXPB_Day_4: match data to get outliers

cc = 1;
for i = 1:length(invasive_B4)
    tmp_name = invasive_B4{i,2}(1:end-1);
    if ~contains(tmp_name,surface_B4(:,2))
%         disp(tmp_name)
        rm_idx_B4W(cc) = i;
        cc = cc+1;
    end
end

%%
cc = 1;
for i = 1:length(surface_B4)
    tmp_name = surface_B4{i,2}+"w";
    if ~contains(tmp_name,invasive_B4(:,2))
%         disp(tmp_name)
        rm_idx_B4S(cc) = i;
        cc = cc+1;
    end
end

%% remove mismatched files

if length(invasive_B4) == 227
    invasive_B4(rm_idx_B4W,:) = [];
end

if length(surface_B4) == 222
    surface_B4(rm_idx_B4S,:) = [];
end

if length(invasive_B4) == 207
    invasive_B4([121,122,128],:) = [];
end

%%

load("invasive_B6.mat");
load("surface_B6.mat");

%% EXPB_Day_6: match data to get outliers
cc = 1;
for i = 1:length(invasive_B6)
    tmp_name = invasive_B6{i,2}(1:end-1);
    if ~contains(tmp_name,surface_B6(:,2))
%         disp(tmp_name)
        rm_idx_B6W(cc) = i;
        cc = cc+1;
    end
end

%%
cc = 1;
for i = 1:length(surface_B6)
    tmp_name = surface_B6{i,2}+"w";
    if ~contains(tmp_name,invasive_B6(:,2))
        disp(tmp_name)
        rm_idx_B6S(cc) = i;
        cc = cc+1;
    end
end
%%
if length(invasive_B6) == 276
    invasive_B6(rm_idx_B6W,:) = [];
end

if length(surface_B6) == 288
    surface_B6(rm_idx_B6S,:) = [];
end

if length(surface_B6) == 273
    surface_B6(171,:) = [];
end
%% EXPB_Day_4: tabulate data

NB4 = length(invasive_B4);

tab_B4 = cell(NB4,11);

for i = 1:NB4

    % 1. full name or ID
    if contains(invasive_B4{i,2},'796')
        tab_B4{i,1} = "parent";
    else
        tab_B4{i,1} = invasive_B4{i,2}(1:3);
    end
    
    % 2. suphide (Y/N)
    if contains(invasive_B4{i,2},'400S')
        tab_B4{i,2} = 'Y';
    else
        tab_B4{i,2} = 'N';
    end
    
    tmp_idx = find(contains(surface_B4(:,2),invasive_B4{i,2}(1:end-1)));
    
    % 3. surface area 
    Itmp = surface_B4{tmp_idx,1};
    tab_B4{i,3} = sum(sum(Itmp<0.5));
    
    % 4. washed area
    Jtmp = invasive_B4{i,1};
    tab_B4{i,4} = sum(sum(Jtmp));
    
    % 5. Area ratio
    tab_B4{i,5} = tab_B4{i,4}/tab_B4{i,3};
    
    % 6. Time of trial 
    tab_B4{i,6} = "Dec_10";
    
    % 7. nutrient: BD50 or BD75
    if contains(invasive_B4{i,2},'BD50')
        tab_B4{i,7} = "BD50";
    elseif contains(invasive_B4{i,2},'BD75')
        tab_B4{i,7} = "BD75";
    end
    
    % 8. SLAD: 1x or 2x 
    if contains(invasive_B4{i,2},'1xSLAD')
        tab_B4{i,8} = "1xSLAD";
    elseif contains(invasive_B4{i,2},'2xSLAD')
        tab_B4{i,8} = "2xSLAD";
    end 
    
    % 9. Number of days: 3,4 or 6
    tab_B4{i,9} = "day4";
    
    % 10. Experiment type: A,B,C,D or E
    tab_B4{i,10} = "ExpB";

    % 11. Full name
    tab_B4{i,11} = invasive_B4{i,2}(1:end-1);
end

%% EXPB_Day_4: concantenate to a single table 

Id = 1:length(tab_B4);

df_B4 = table;

df_B4.yeast_id = Id';
df_B4.mutant_id = tab_B4(:,1);
df_B4.sulfide = tab_B4(:,2);
df_B4.surface_area = tab_B4(:,3);
df_B4.washed_area = tab_B4(:,4);
df_B4.area_ratio = tab_B4(:,5);
df_B4.time_of_exp = tab_B4(:,6);
df_B4.nutrient = tab_B4(:,7);
df_B4.slad = tab_B4(:,8);
df_B4.day = tab_B4(:,9);
df_B4.exp_type = tab_B4(:,10);
df_B4.full_name = tab_B4(:,11);

%% EXPB_Day_4: write table to csv

% writetable(df_B4,'../raw_data/image_data/slad_expB_day4.csv')

%% EXPB_Day_6: tabulate data

NB6 = length(invasive_B6);

tab_B6 = cell(NB6,11);

for i = 1:NB6

    % 1. full name or ID
    if contains(invasive_B6{i,2},'796')
        tab_B6{i,1} = "parent";
    else
        tab_B6{i,1} = invasive_B6{i,2}(1:3);
    end

    % 2. suphide (Y/N)
    if contains(invasive_B6{i,2},'400S')
        tab_B6{i,2} = 'Y';
    else
        tab_B6{i,2} = 'N';
    end
    
    tmp_idx = find(contains(surface_B6(:,2),invasive_B6{i,2}(1:end-1)));
    
    % 3. surface area 
    Itmp = surface_B6{tmp_idx,1};
    tab_B6{i,3} = sum(sum(Itmp<0.5));
    
    % 4. washed area
    Jtmp = invasive_B6{i,1};
    tab_B6{i,4} = sum(sum(Jtmp));
    
    % 5. Area ratio
    tab_B6{i,5} = tab_B6{i,4}/tab_B6{i,3};
    
    % 6. Time of trial 
    tab_B6{i,6} = "Dec_12";
    
    % 7. nutrient: BD50 or BD75
    if contains(invasive_B6{i,2},'BD50')
        tab_B6{i,7} = "BD50";
    elseif contains(invasive_B6{i,2},'BD75')
        tab_B6{i,7} = "BD75";
    end
    
    % 8. SLAD: 1x or 2x 
    if contains(invasive_B6{i,2},'1xSLAD')
        tab_B6{i,8} = "1xSLAD";
    elseif contains(invasive_B6{i,2},'2xSLAD')
        tab_B6{i,8} = "2xSLAD";
    end 
    
    % 9. Number of days: 3,4 or 6
    tab_B6{i,9} = "day6";
    
    % 10. Experiment type: A,B,C,D or E
    tab_B6{i,10} = "ExpB";

    % 11. Full name
    tab_B6{i,11} = invasive_B6{i,2}(1:end-1);
end

%% EXPB_Day_6: concantenate to a single table 

Id = 1:length(tab_B6);

df_B6 = table;

df_B6.yeast_id = Id';
df_B6.mutant_id = tab_B6(:,1);
df_B6.sulfide = tab_B6(:,2);
df_B6.surface_area = tab_B6(:,3);
df_B6.washed_area = tab_B6(:,4);
df_B6.area_ratio = tab_B6(:,5);
df_B6.time_of_exp = tab_B6(:,6);
df_B6.nutrient = tab_B6(:,7);
df_B6.slad = tab_B6(:,8);
df_B6.day = tab_B6(:,9);
df_B6.exp_type = tab_B6(:,10);
df_B6.full_name = tab_B6(:,11);

%% EXPB_Day_6: write table to csv

% writetable(df_B6,'../raw_data/image_data/slad_expB_day6.csv')
