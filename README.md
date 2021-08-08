# Staff-Retention-Analysis

**PERSONAL AND/OR UNIQUE IDENTIFIERS HAVE BEEN REMOVED FROM ALL SAMPLE DATA** 

Conducted a staff retention analysis for the entire network that included all the companies, entities, and departments within the network.

**Goal and Purpose:**

Discussed goals and direction with cabinet level colleagues to conceive and implement a Tableau report that looked at staff retention rates for the 19-20 fiscal year. The company wanted to see whether the various personal and professional implications of the COVID-19 pandemic had any significant correlations with staff retention. Level of detail in data granularity was also of importance as stakeholders requested to see retention rates broken down by years of service, employment status, and demographic information. Utilized NLP

**Methodology:**

As I was the lead data scientist for this project, I decided to run a **point in time vs point in time** retention analysis. I took all the employees that were in their jobs in December of 2019 and then compared their employment status in December of 2020. If their employee IDs (unique identifiers) were still in the SQL data pull I conducted in 2020, that indicated to me that they were retained, if not, the employee was either terminated or resigned. The best way to visualize this retention data was to link the data to our Tableau servers. This allowed all stakeholders to view and interact with the data according to their own preferences. 

**Final Report:**

The final report had many visualizations. One of which was a macro-level view that displays staff retention for all companies within the network broken down by years of service, full-time/part-time employees, and self-identified demomgraphic information.

![Visualization 1](Retention-1.gif)



The second visualization displays a breakdown of the staff retention rates by departments within the network. The data can be partitioned along demographic lines, years of service, but also by job title and seniority.

![Visualization 2](Retention-2.gif)
