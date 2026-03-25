% match parent invasive with surface growth dataset
% 
% Kai Li
% 25 Mar 2026

clear
clc

load("invasive_E4.mat");
load("surface_E4.mat");

%% EXPE_Day_4: match data to get outliers

cc = 1;
for i = 1:length(invasive_E4)
    tmp_name = invasive_E4{i,3}(1:end-1);
    if ~contains(tmp_name,surface_E4(:,3))
%         disp(tmp_name)
        rm_idx_E4W(cc) = i;
        cc = cc+1;
    end
end

%%
cc = 1;
for i = 1:length(surface_E4)
    tmp_name = surface_E4{i,3}+"w";
    if ~contains(tmp_name,invasive_E4(:,3))
%         disp(tmp_name)
        rm_idx_E4S(cc) = i;
        cc = cc+1;
    end
end

%% remove mismatched files

if length(invasive_E4) == 210
    invasive_E4(rm_idx_E4W,:) = [];
end

if length(surface_E4) == 223
    surface_E4(rm_idx_E4S,:) = [];
end


%%

load("invasive_E6.mat");
load("surface_E6.mat");

%% EXPE_Day_6: match data to get outliers
cc = 1;
for i = 1:length(invasive_E6)
    tmp_name = invasive_E6{i,3}(1:end-1);
    if ~contains(tmp_name,surface_E6(:,3))
%         disp(tmp_name)
        rm_idx_E6W(cc) = i;
        cc = cc+1;
    end
end

%%
cc = 1;
for i = 1:length(surface_E6)
    tmp_name = surface_E6{i,3}+"w";
    if ~contains(tmp_name,invasive_E6(:,3))
%         disp(tmp_name)
        rm_idx_E6S(cc) = i;
        cc = cc+1;
    end
end
%%
if length(invasive_E6) == 212
    invasive_E6(rm_idx_E6W,:) = [];
end

if length(surface_E6) == 220
    surface_E6(rm_idx_E6S,:) = [];
end

%% EXPE_Day_4: tabulate data

NE4 = length(invasive_E4);

tab_E4 = cell(NE4,11);

for i = 1:NE4
    
    % 1. full name or ID
    if invasive_E4{i,3}(5:6) == "BD"
        tab_E4{i,1} = "parent";
    else
        tab_E4{i,1} = invasive_E4{i,3}(5:8);
    end
    
    % 2. suphide (Y/N)
    if contains(invasive_E4{i,3},'400S')
        tab_E4{i,2} = 'Y';
    else
        tab_E4{i,2} = 'N';
    end
    
    tmp_idx = find(contains(surface_E4(:,3),invasive_E4{i,3}(1:end-1)));
    
    % 3. surface area 
    Itmp = surface_E4{tmp_idx,1};
    tab_E4{i,3} = sum(sum(Itmp<0.5));
    
    % 4. washed area
    Jtmp = invasive_E4{i,1};
    tab_E4{i,4} = sum(sum(Jtmp));
    
    % 5. Area ratio
    tab_E4{i,5} = tab_E4{i,4}/tab_E4{i,3};
    
    % 6. Time of trial 
    tab_E4{i,6} = "Nov_26";
    
    % 7. nutrient: BD50 or BD75
    if contains(invasive_E4{i,3},'BD50')
        tab_E4{i,7} = "BD50";
    elseif contains(invasive_E4{i,3},'BD75')
        tab_E4{i,7} = "BD75";
    end
    
    % 8. SLAD: 1x or 2x 
    tab_E4{i,8} = nan; 
    
    % 9. Number of days: 3,4 or 6
    tab_E4{i,9} = "day4";
    
    % 10. Experiment type: A,B,C,D or E
    tab_E4{i,10} = "ExpE";

    % 11. Full name
    tab_E4{i,11} = invasive_E4{i,3}(1:end-1);
end

%% EXPE_Day_4: concantenate to a single table 

Id = 1:length(tab_E4);

df_E4 = table;

df_E4.yeast_id = Id';
df_E4.mutant_id = tab_E4(:,1);
df_E4.sulfide = tab_E4(:,2);
df_E4.surface_area = tab_E4(:,3);
df_E4.washed_area = tab_E4(:,4);
df_E4.area_ratio = tab_E4(:,5);
df_E4.time_of_exp = tab_E4(:,6);
df_E4.nutrient = tab_E4(:,7);
df_E4.slad = tab_E4(:,8);
df_E4.day = tab_E4(:,9);
df_E4.exp_type = tab_E4(:,10);
df_E4.full_name = tab_E4(:,11);

%% EXPE_Day_4: write table to csv

writetable(df_E4,'../raw_data/image_data/slad_expE_day4.csv')

%% EXPE_Day_6: tabulate data

NE6 = length(invasive_E6);

tab_E6 = cell(NE6,11);

for i = 1:NE6

    % 1. full name or ID
    if invasive_E6{i,3}(5:6) == "BD"
        tab_E6{i,1} = "parent";
    else
        tab_E6{i,1} = invasive_E6{i,3}(5:8);
    end
    
    % 2. suphide (Y/N)
    if contains(invasive_E6{i,3},'400S')
        tab_E6{i,2} = 'Y';
    else
        tab_E6{i,2} = 'N';
    end
    
    tmp_idx = find(contains(surface_E6(:,3),invasive_E6{i,3}(1:end-1)));
    
    % 3. surface area 
    Itmp = surface_E6{tmp_idx,1};
    tab_E6{i,3} = sum(sum(Itmp<0.5));
    
    % 4. washed area
    Jtmp = invasive_E6{i,1};
    tab_E6{i,4} = sum(sum(Jtmp));
    
    % 5. Area ratio
    tab_E6{i,5} = tab_E6{i,4}/tab_E6{i,3};
    
    % 6. Time of trial 
    tab_E6{i,6} = "Nov_26";
    
    % 7. nutrient: BD50 or BD75
    if contains(invasive_E6{i,3},'BD50')
        tab_E6{i,7} = "BD50";
    elseif contains(invasive_E6{i,3},'BD75')
        tab_E6{i,7} = "BD75";
    end
    
    % 8. SLAD: 1x or 2x 
    tab_E6{i,8} = nan;
    
    % 9. Number of days: 3,4 or 6
    tab_E6{i,9} = "day6";
    
    % 10. Experiment type: A,B,C,D or E
    tab_E6{i,10} = "ExpE";

    % 11. Full name
    tab_E6{i,11} = invasive_E6{i,3}(1:end-1);
end

%% EXPE_Day_6: concantenate to a single table 

Id = 1:length(tab_E6);

df_E6 = table;

df_E6.yeast_id = Id';
df_E6.mutant_id = tab_E6(:,1);
df_E6.sulfide = tab_E6(:,2);
df_E6.surface_area = tab_E6(:,3);
df_E6.washed_area = tab_E6(:,4);
df_E6.area_ratio = tab_E6(:,5);
df_E6.time_of_exp = tab_E6(:,6);
df_E6.nutrient = tab_E6(:,7);
df_E6.slad = tab_E6(:,8);
df_E6.day = tab_E6(:,9);
df_E6.exp_type = tab_E6(:,10);
df_E6.full_name = tab_E6(:,11);

%% EXPE_Day_6: write table to csv

writetable(df_E6,'../raw_data/image_data/slad_expE_day6.csv')
